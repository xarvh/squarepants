// TODO I think every time I run `eval()` it rewrites squarepantsMain?
// Maybe I should really bit the bullet and use a server.
const compiler = squarepantsMain;


window.onload = () => {

    update();

    const canvas = document.getElementById('output');
    canvas.addEventListener('mouseleave', (e) => console.log('mouseleave'));
    canvas.addEventListener('mousemove', onMouseMoveOverCanvas);
}


const decs = (n) =>
    Math.round(n * 100) / 100;



function onMouseMoveOverCanvas(e) {
    // -1 because we want coordinates to be from 0 to 1 inclusive
    const w = e.target.clientWidth - 1;
    const h = e.target.clientHeight - 1;
    const x = e.offsetX / w;
    const y = 1 - e.offsetY / h;
    console.log('mm', decs(x), decs(y));
}


function update() {

    const textarea = document.getElementById('input');

    let shaderFunction = null;

    let result;

    try {
        result = compiler(textarea.value);
    } catch(e) {
        console.error('ERROR:', e);
        result = null;
        error = 'compilerError: ' + e.message;
    }

    if (result)
        if (result[0] === 'Ok') {
            shaderFunction = eval(result[1]);
            error = '';
        } else {
            error = result[1];
        }


    const e = document.getElementById('error');
    e.innerHTML = error ? 'ERROR! ---------------\n' + error : '';

    if (shaderFunction) {
        const r = updateCanvas(shaderFunction);
        if (r) e.textContent = r.message;
    }
}



function updateCanvas(shaderFunction) {

    const canvas = document.getElementById('output');

    const w = canvas.width;
    const h = canvas.height;

    const ctx = canvas.getContext('2d');
    const imageData = ctx.createImageData(w, h);

    for (let x = 0; x < w; x++) for (let y = 0; y < h; y++) {

        let frag;
        try {
            frag = shaderFunction(x / (w - 1))(1 - y / (h - 1));
        } catch (e) {
            return e;
        }

        let j = (x + y * w) * 4;
        imageData.data[j + 0] = frag.r * 255;
        imageData.data[j + 1] = frag.g * 255;
        imageData.data[j + 2] = frag.b * 255;
        imageData.data[j + 3] = 255;
    }

    ctx.putImageData(imageData, 0, 0);
    return null;
}
