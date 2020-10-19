const CommentsAndStrings = require('./src/CommentsAndStrings');
const SortIndent = require('./src/SortIndent');


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










function chunkToString(c) {
// return { t: '' + c.type, s: inp.slice(c.start, c.end) };
 return inp.slice(c.start, c.end);
}


let chunks = CommentsAndStrings.parseCommentsAndStrings(inp);
let lines = CommentsAndStrings.groupChunksIntoLines(inp, chunks);
let tree = SortIndent.sortIndent(lines);


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
