// capture.js - Robust event handler using MutationObserver

console.log("Capture script running. Setting up observer...");

document.addEventListener("DOMContentLoaded", () => {
    
    // --- 1. Event Attachment Function ---
    // This function checks for the canvas and attaches the handler if needed.
    function attachPointerLockListener() {
        const canvas = document.querySelector("canvas");
        
        // We check for a custom attribute to ensure we don't attach the handler multiple times
        if (canvas && canvas.getAttribute('data-pointer-locked-ready') !== 'true') { 
            
            console.log("Canvas found. Attaching PERMANENT pointer lock listener.");
            
            canvas.onclick = () => {
                console.log("Requesting pointer lock on user click.");
                // Call requestPointerLock only if not already locked
                if (document.pointerLockElement !== canvas) {
                    canvas.requestPointerLock();
                }
            };
            
            // Mark the canvas so the observer knows this job is done for this element
            canvas.setAttribute('data-pointer-locked-ready', 'true');
        }
    }

    // --- 2. Initial Check ---
    // Try once in case the canvas is already in the DOM
    attachPointerLockListener();

    // --- 3. Set up the Observer ---
    const observer = new MutationObserver((mutationsList, observer) => {
        // We only need to check if children were added or removed (childList)
        for (const mutation of mutationsList) {
             if (mutation.type === 'childList') {
                 // Re-check and re-attach when any change happens in the body
                 attachPointerLockListener(); 
                 return; // Only need to process once per change batch
             }
        }
    });

    // Start observing the <body> tag (the highest common parent)
    observer.observe(document.body, { childList: true, subtree: true });
    
    // Note: If you still have the `app.ports.requestPointerLock.subscribe` code in this file, 
    // you can safely remove it now, as this observer is the new, superior trigger.
});