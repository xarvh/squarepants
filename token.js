
/*
1. How does the AST represent comments?

*/



inp = `
something
  something indented
  something {- with comment -} indented
    and moar indentation


a pathological
  case {-
-} that should throw an error
    or not?



b {- inline comment -} "hhhhhh"
  indented
    moarindented
{- preceding comment
{- preceding comment
-}
-}
    b -- blah
  b


bb
  b
`


const chunkType = {
  ContentLine: 'l',
  InLineComment: 'ic',
  FullLineComment: 'fc',
  // TODO: remove hacky way of keeping track of multiline comments nesting depth.
  MultiLineComment: 1,
  SoftQuotedString: 'ss',
  HardQuotedString: 'hs',
}



function searchTowardsRoot(leafToAdd, targetLeaf) {

    if (leafToAdd.indent < targetLeaf.indent) {
      if (leafToAdd.indent > targetLeaf.parent.indent) {
        throw new Error('bad indent!');
      }

      searchTowardsRoot(leafToAdd, targetLeaf.parent);

    } else if (leafToAdd.indent === targetLeaf.indent) {
      append(leafToAdd, targetLeaf.parent);
    } else {
      append(leafToAdd, targetLeaf);
    }
  }

function append(leafToAdd, parent) {
    leafToAdd.parent = parent;
    parent.children.push(leafToAdd);
  }



function getLineIndent(chunks) {
    // TODO find a less dumb algorithm
    let l = chunks.map(chunkToString).join('');
    for (var i = 0; l[i] === ' '; i++);
    return i;
  }



function chunkIsEmptyOrComment(chunk) {
  return (chunk.type !== chunkType.ContentLine) || /^[ ]*\n?$/.test(chunkToString(chunk));
}


function sortIndent(lines) {

  let root = {
    chunks: [],
    parent: null,
    indent: -1,
    // TODO make clear that these lines *precede* the leaf?
    commentLines: [],
    children: [],
  };

  let lastAdded = root;
  let commentLinesAccumulator = [];

  lines.forEach(chunks => {
    if (chunks.every(chunkIsEmptyOrComment)) {
      commentLinesAccumulator.push(chunks);
    } else {

      let leaf = {
        chunks: chunks,
        indent: getLineIndent(chunks),
        parent: null,
        commentLines: [],
        children: [],
      };

      searchTowardsRoot(leaf, lastAdded);
      leaf.commentLines = commentLinesAccumulator;
      commentLinesAccumulator = [];
      lastAdded = leaf;
    }
  });

  return root;
}







function groupChunksIntoLines(code, chunks) {
  // TODO merge with parseCommentsAndStrings()?

  let lines = [];
  let lastLine = [];

  chunks.forEach(c => {
    lastLine.push(c);
    if ([chunkType.ContentLine, chunkType.FullLineComment].includes(c.type) && code[c.end - 1] === '\n') {
      lines.push(lastLine);
      lastLine = [];
    }
  });

  if (lastLine.length)
    lines.push(lastLine);

  return lines;
}





function parseCommentsAndStrings(code) {

  let chunks = [];

  let currentChunk = {
    type: chunkType.ContentLine,
    start: 0,
    end: 0,
  };

  function newChunk(newType) {

    if (newType > -1 && currentChunk.type > -1) {
      // We're inside a multi-line comment, so just keep track of depth.
      currentChunk.type = newType;
      return;
    }

    // Don't create multiple chunks for nested multiline comments
    if (newType === chunkType.MultiLineComment && newType === currentChunk.type)
      return;

    let newChunk = {
      type: newType,
      start: currentChunk.end,
      end: currentChunk.end,
    }

    if (currentChunk.end > currentChunk.start)
      chunks.push(currentChunk);
    currentChunk = newChunk;
  }


  function test(s) {
    for (let i = 0; i < s.length; i++)
      if (code[currentChunk.end + i] !== s[i])
        return false;
    return true;
  }

  // if the token marks the start of a new context, it belongs to the NEW chunk
  function test_changeContext_thenConsume(token, blockType) {
    if (!test(token)) return false;

    newChunk(blockType);
    currentChunk.end += token.length;
    return true;
  }

  // if the token marks the end of a context, it belongs to the OLD chunk
  function test_consume_thenChangeContext(token, blockType) {
    if (!test(token)) return false;

    currentChunk.end += token.length;
    newChunk(blockType);
    return true;
  }



  const testForStartOfMultiComment = () =>
    test_changeContext_thenConsume('{-', (+currentChunk.type || 0) + 1);

  const testForEndOfMultiComment = () =>
    test_consume_thenChangeContext('-}',
      +currentChunk.type > 0
        ? (currentChunk.type === 1 ? chunkType.ContentLine : currentChunk.type - 1)
        : new Error('-} without {- =(')
    );

  function run(...fs) {
    if (!fs.some(f => f())) currentChunk.end += 1;
  }

  while (currentChunk.end < code.length) {

    switch (currentChunk.type) {
      case chunkType.ContentLine:
        // TODO error on tabs

        run(
          testForStartOfMultiComment,
          () => test_consume_thenChangeContext('\n', chunkType.ContentLine),
          () => test_changeContext_thenConsume('--', chunkType.FullLineComment),
          () => test_changeContext_thenConsume('"""', chunkType.HardQuotedString),
          () => test_changeContext_thenConsume('"', chunkType.SoftQuotedString),
        );
        break;
      case chunkType.FullLineComment:
        run(() => test_consume_thenChangeContext('\n', chunkType.ContentLine));
        break;
      case chunkType.SoftQuotedString:
        // TODO cannot contain \n?
        // TODO error if you meet a triple string?
        run(() => test_consume_thenChangeContext('"', chunkType.ContentLine));
        break
      case chunkType.HardQuotedString:
        run(() => test_consume_thenChangeContext('"""', chunkType.ContentLine));
        break
      default:
        run(
          testForStartOfMultiComment,
          testForEndOfMultiComment
        );
        break;
    }
  }

  chunks.push(currentChunk);
  return chunks;
}








function chunkToString(c) {
// return { t: '' + c.type, s: inp.slice(c.start, c.end) };
 return inp.slice(c.start, c.end);
}


let chunks = parseCommentsAndStrings(inp);
let lines = groupChunksIntoLines(inp, chunks);
let tree = sortIndent(lines);


function descend(indent, node) {

  const chunksToString = (cs) => cs.map(chunkToString).join('').replace('\n', '..');

  let i = '    '.repeat(Math.max(0, indent));
  let s = chunksToString(node.chunks);
  node.commentLines.forEach(cs => console.log(chunksToString(cs)));
  console.log(i + s.trim());
  node.children.forEach(c => descend(indent + 1, c));
}


//console.log(chunks.map(chunkToString))
//lines.forEach(l => console.log(getLineIndent(l), l.map(chunkToString)));
descend(-1, tree)
