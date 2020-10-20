








function ContentLineToTokens(chunkAsString) {

  const trimmed = chunkAsString.trimStart();
  if (!trimmed) return

  let types = [{
    regex: /^[a-zA-Z._][a-zA-Z._0-9]*/,
    id: 'Word',
  }, {
    regex: /^[,()\[\]{}]/,
    id: 'Parens',
  }, {
    regex: /^[=+\-*/:><!^|]/,
    id: 'Operator',
  }];


  const re = () => {
    const type = types.pop();
    if (!type) throw new Error('unrecognised token "' + trimmed + '"');

    const match = trimmed.match(type.regex);
    if (!match) return re();

    return {
      content: match[0],
      type: type.id,
      rest: trimmed.slice(match[0].length),
    };
  }

  return re();
}





console.log(ContentLineToTokens(' asdf + - sdf.asasd_asdf'))











/*


//Se riduco a token, poi posso scasinare con il formato come cazzo mi pare e rendere piú facile ?
//tokenisation is mostly to PRODUCE ERROR MESSAGES
//break down control structures, whatever is left are expressions/commands?

inp = `

  if expr then
      incompleteExpressionOrCommand
        remainderOfExpressionOrCommand
  else
      case expr of
          pattern ->
              exprOrCommand
          anotherPattern ->
              exprOrCommand
          _ ->
              exprOrCommand

`

[ 'Uppercase', 'op:ModuleAccess', 'lowercase', 'parens:Open', 'literal:Int', 'op:Plus', 'literal:Int'



function makeInitialState() {

  let root = {
    type: 'root',
    children: [],
    // TODO start/end?
  };

  return {
    position: 0,
    root,
    lastAdded: root,
  }
}



function resolveNesting(state, chunk) {

  switch (toString(chunk)) {
    case "if":
      add another node, no problem

    case "then":
      assert: parent MUST be an "if"
      open a "then" child for the "if" node

    case "else":
      assert parent MUST be an "if" AND MUST have a "then" already populated

    case "indentation up":
      if parent is an "if/then": go out of the "if/then"





  }

}









*/
