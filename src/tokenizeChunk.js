
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
    root,
    lastAdded: root,
  }
}


function resolveNesting(state, chunk) {



}










