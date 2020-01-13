import Two from 'two.js';
    
const elem = document.getElementById('root');
const two = new Two({ width: 275, height: 200 }).appendTo(elem);

const draw = (e) => {
    const data = JSON.parse(e.data);
    console.log(data);
    const rect = two.makeRectangle(213, 100, 100, 100);
    rect.fill = 'rgb(0, 200, 255)';
    rect.opacity = 0.75;
    rect.noStroke();
    two.update();
};

class App {
  constructor() {}

  run() {
    console.log("Start running events");

    if (!window.EventSource)
      alert("You're browser does not support EventSource needed for this page");

    const eventSource = new EventSource("/start");

    eventSource.addEventListener('data', draw);

    eventSource.addEventListener("gameover", function(e) {
      console.log("gameover");
    });
  }
}

export default App;
