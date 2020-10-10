
function run(grammar, code) {

  let parser;
  try {
    parser = peg.generate(grammar);
  } catch (e) {
    return { code: "nope", grammar: e.message };
  }

  let parsed;
  try {
    parsed = parser.parse(code);
  } catch (e) {
    return { code: e.message, grammar: 'Ok' };
  }

  return { code: JSON.stringify(parsed, null, 2), gramamr: 'Ok' }
}


window.onload = () => {
  const grammarTextarea = document.querySelector('.grammar textarea');
  const codeTextarea = document.querySelector('.code textarea');
  const grammarP = document.querySelector('.grammar .output');
  const codeP = document.querySelector('.code .output');

  grammarTextarea.addEventListener('input', onChange);
  codeTextarea.addEventListener('input', onChange);

  grammarTextarea.value = localStorage.grammar;
  codeTextarea.value = localStorage.code;
  onChange();

  function onChange() {
    const g = grammarTextarea.value;
    const c = codeTextarea.value;
    localStorage.grammar = g;
    localStorage.code = c;

    const result = run(g, c);
    grammarP.innerHTML = result.grammar;
    codeP.innerHTML = result.code;
  }
}

