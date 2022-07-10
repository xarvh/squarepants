
window.onload = () => update();



function update() {
    const textarea = document.getElementById('input');

    let shaderFunction = null;

    try {
      shaderFunction = eval(textarea.value + '\ncoloraPixel;');
      error = '';
    } catch (e) {
      error = e.message;
    }

    const e = document.getElementById('error');
    e.textContent = error;

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
            frag = shaderFunction(x / (w - 1), 1 - y / (h - 1));
        } catch (e) {
            return e;
        }

        let j = (x + y * w) * 4;
        imageData.data[j + 0] = frag.rosso * 255;
        imageData.data[j + 1] = frag.verde * 255;
        imageData.data[j + 2] = frag.blu * 255;
        imageData.data[j + 3] = 255;
    }

    ctx.putImageData(imageData, 0, 0);
    return null;
}
