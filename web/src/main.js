console.log("outside");
window.addEventListener('load', function() {
   console.log("inside");
   if (!window.EventSource)
        alert("You're browser does not support EventSource needed for this page ");

    const eventSource = new EventSource("/start");
    eventSource.onmessage = function(e) {
        console.log("data, ", e.data);
    }
}, false);
