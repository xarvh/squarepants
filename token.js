inp = `

b {- -}

{- --
    {-
    -}
-}
   b
   -- blah
  b
  -- {-

b{- bb-}b
  b
`


function parseComments(code) {
  let chunks = [];

  let currentContext = 'content';

  let currentChunk = {
    type: 'u',
    start: 0,
    end: 0,
  };

  function newContext(context) {
    currentContext = context;

    let newType;
    switch (context) {
      case 'content': newType = 'u'; break;
      case 'line-comment': newType = 'l'; break;
      default: newType = 'm'; break;
    }

    if (newType === currentChunk.type)
      return;

    let newChunk = {
      type: newType,
      start: currentChunk.end,
      end: currentChunk.end,
    }

    // TODO push only if start > end
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
  function test_changeContext_thenConsume(token, context) {
    if (!test(token)) return false;

    newContext(context);
    currentChunk.end += token.length
    return true;
  }

  // if the token marks the end of a context, it belongs to the OLD chunk
  function test_consume_thenChangeContext(token, context) {
    if (!test(token)) return false;

    console.error('end', chunkToString(currentChunk))
    currentChunk.end += token.length
    newContext(context);
    return true;
  }



  const testForStartOfLineComment = () =>
    test_changeContext_thenConsume('--', 'line-comment');

  const testForEndOfLineComment = () =>
    test_consume_thenChangeContext('\n', 'content');

  const testForStartOfMultiComment = () =>
    test_changeContext_thenConsume('{-', (+currentContext || 0) + 1);

  const testForEndOfMultiComment = () =>
    test_consume_thenChangeContext('-}',
      +currentContext > 0
        ? (currentContext === 1 ? 'content' : currentContext - 1)
        : new Error('-} without {- =(')
    );


  function run(...fs) {
    if (!fs.some(f => f())) currentChunk.end += 1;
  }

  while (currentChunk.end < code.length) {

    switch (currentContext) {
      case 'content':
        run(testForStartOfLineComment, testForStartOfMultiComment);
        break;
      case 'line-comment':
        run(testForEndOfLineComment);
        break;
      default:
        run(testForStartOfMultiComment, testForEndOfMultiComment);
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
 return { t: c.type, s: inp.slice(c.start, c.end) };
}


console.log(parseComments(inp).map(chunkToString))
