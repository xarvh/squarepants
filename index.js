
const initialGrammar = `
grammar!
`


const initialCode = `
code!
`



function run(grammar, code) {

  try {
    const parser = peg.generate(grammar);
  } catch (e) {
    return { code: "nope", grammar: e.message };
  }

  try {
    const parsed = parser.parse(code);
  } catch (e) {
    return { code: e.message, grammar: 'Ok' };
  }

  return { code: parsed, gramamr: 'Ok' }
}


window.onload = () => {
  const grammarTextarea = document.querySelector('.grammar textarea');
  const codeTextarea = document.querySelector('.code textarea');
  const grammarP = document.querySelector('.grammar p');
  const codeP = document.querySelector('.code p');

  grammarTextarea.addEventListener('input', onChange);
  codeTextarea.addEventListener('input', onChange);

  grammarTextarea.innerText = initialGrammar;
  codeTextarea.innerText = initialCode;
  onChange();

  function onChange() {
    const result = run(grammarTextarea.innerText, codeTextarea.innerText);
    grammarP.innerText = result.grammar;
    codeP.innerText = result.code;
  }
}

