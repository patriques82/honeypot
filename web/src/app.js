class App {
  constructor() {}

  run() {
    console.log("Start running events");
    if (!window.EventSource)
      alert("You're browser does not support EventSource needed for this page");

    const eventSource = new EventSource("/start");
    eventSource.addEventListener("data", function(e) {
      console.log("data, ", e.data);
    });
    eventSource.addEventListener("gameover", function(e) {
      console.log("gameover");
    });
  }
}

export default App;
