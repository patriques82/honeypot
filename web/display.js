window.addEventListener('load', function() {
   if (!window.EventSource)
        alert("You're browser does not support EventSource needed for this page ");

    const eventSource = new EventSource("/start");
    eventSource.onmessage = function(msg) {
        console.log("data");
    }
}, false);
