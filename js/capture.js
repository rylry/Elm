var app = Elm.Main.init({ node: document.getElementById('elm') });

app.ports.requestPointerLock.subscribe(() => {
    // Wait for Elm/WebGL to finish rendering the canvas element
    requestAnimationFrame(() => {
        const canvas = document.querySelector("canvas");

        if (!canvas) {
            console.error("Canvas not found!");
            return;
        }

        canvas.onclick = () => {
            console.log("requesting pointer lock");
            canvas.requestPointerLock();
        };
    });
});