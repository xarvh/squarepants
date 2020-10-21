const CommentsAndStrings = require('./src/CommentsAndStrings');
const SortIndent = require('./src/SortIndent');
const TokenizeChunk = require('./src/tokenizeChunk');


/*

    1. How does the AST represent comments?

*/

inp = `
rectFragmentShader : Attributes -> Uniforms -> Varying -> Maybe Color
rectFragmentShader attributes uniforms varying =

    -- TODO: transform into pixelSize, make it a uniform

    pixelsPerTile =
        30.0

    e =
        0.5 / pixelsPerTile

    {-
     -     0               1                            1                     0
     -     |------|--------|----------------------------|----------|----------|
     -  -edge-e  -edge  -edge+e                      edge-e      edge      edge+e
     -}
    mirrorStep : Float -> Float -> Float
    mirrorStep edge p =
        (smoothstep (-edge - e) (-edge + e), p) - (smoothstep (edge - e) (edge + e) p)

    strokeSize =
        uniforms.dimensions / 2.0 + uniforms.strokeWidth

    fillSize =
        uniforms.dimensions / 2.0 - uniforms.strokeWidth

    alpha =
        (mirrorStep strokeSize.x localPosition.x) * (mirrorStep strokeSize.y localPosition.y)

    strokeVsFill =
        (mirrorStep fillSize.x localPosition.x) * (mirrorStep fillSize.y localPosition.y)

    color =
        mix stroke fill strokeVsFill

    return Just <| opacity * alpha * (vec4 color 1.0)
`


_inp = `
something
  something indented
  something {- with comment -} indented
    and moar indentation


a pathological
  case {-
-} that should throw an error
    or not



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
 return inp.slice(c.start, c.end);
}


let chunks = CommentsAndStrings.parseCommentsAndStrings(inp);
let lines = CommentsAndStrings.groupChunksIntoLines(inp, chunks);
let indentTree = SortIndent.sortIndent(lines);
let tokenTree = TokenizeChunk.indentTreeToTokenTree(inp, indentTree);


function descend(indent, node) {

  const chunksToString = (cs) => cs.map(chunkToString).join('').replace('\n', '..');

  let i = '    '.repeat(Math.max(0, indent));
  let s = chunksToString(node.chunks);

  console.log(TokenizeChunk.lineChunksToTokens(inp, node.chunks));
//  node.commentLines.forEach(cs => console.log(chunksToString(cs)));

//  console.log(i + s.trim());
//  console.log(node.chunks.map(chunkToString));
  node.children.forEach(c => descend(indent + 1, c));
}


//console.log(chunks.map(chunkToString))
//lines.forEach(l => console.log(getLineIndent(l), l.map(chunkToString)));
//descend(-1, indentTree)
console.log(JSON.stringify(tokenTree, null, 2));
