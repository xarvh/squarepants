
/*
1. How does the AST represent comments?

2. what if I pretend that comments are spaces?

1. -> if the last line in a Content chunk
        is not empty
        and
        is followed by a comment

   -> if the first line in a Content chunk
        is not empty
        and
        is preceded by a comment

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
  Content: 'u',
  InLineComment: 'ic',
  FullLineComment: 'fc',
  // TODO: remove hacky way of keeping track of multiline comments nesting depth.
  MultiLineComment: 1,
  SoftQuotedString: 'ss',
  HardQuotedString: 'hs',
}




function parseIndentation(chunksIn) {
  let chunksOut = [];

  for (let c = 0; c < chunksIn.length; c++) {

    if chunk is Content
      lines = chunk.content.split('\n')

      take first line
          if there is a comment right before




  }






}








function parseCommentsAndStrings(code) {

  let chunks = [];

  let currentChunk = {
    type: chunkType.Content,
    start: 0,
    end: 0,
  };

  function newChunk(newType) {

    if (newType > -1 && currentChunk.type > -1) {
      // We're inside a multi-line comment, so just keep track of depth.
      currentChunk.type = newType;
      return;
    }

    if (newType === currentChunk.type)
      return;

    let newChunk = {
      type: newType,
      start: currentChunk.end,
      end: currentChunk.end,
    }

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
        ? (currentChunk.type === 1 ? chunkType.Content : currentChunk.type - 1)
        : new Error('-} without {- =(')
    );

  function run(...fs) {
    if (!fs.some(f => f())) currentChunk.end += 1;
  }

  while (currentChunk.end < code.length) {

    switch (currentChunk.type) {
      case chunkType.Content:
        run(
          testForStartOfMultiComment,
          () => test_changeContext_thenConsume('--', chunkType.FullLineComment),
          () => test_changeContext_thenConsume('"""', chunkType.HardQuotedString),
          () => test_changeContext_thenConsume('"', chunkType.SoftQuotedString),
        );
        break;
      case chunkType.FullLineComment:
        run(() => test_consume_thenChangeContext('\n', chunkType.Content));
        break;
      case chunkType.SoftQuotedString:
        // TODO cannot contain \n?
        // TODO error if you meet a triple string?
        run(() => test_consume_thenChangeContext('"', chunkType.Content));
        break
      case chunkType.HardQuotedString:
        run(() => test_consume_thenChangeContext('"""', chunkType.Content));
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






function codeToLines(code) {
  return code
    .split('\n')
    .map((indentedContent, index) => {
      var [s, spaces, content] = indentedContent.match(/([ ]*)(.*)/);
      return {
        lineNumber: index + 1,
        indent: spaces.length,
        content,
      };
    });
}




function parseX(lines) {

  let commentDepth = 0;


  for (let i = 0; i < lines.length; i++) {
    let line = lines[i];

    if (line.startsWith('{-'))
      commentDepth += 1;

    if (commentDepth > 0 || line.startsWith('--'))
      line.isComment = true

    if (line.endsWith('-}'))
      commentDepth -= 1;
  }



}



function chunkToString(c) {
 return { t: '' + c.type, s: inp.slice(c.start, c.end) };
}


console.log(parseCommentsAndStrings(inp).map(chunkToString))
