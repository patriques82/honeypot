import Two from 'two.js';
    
const elem = document.getElementById('root');
const two = new Two({ fullscreen: true }).appendTo(elem);

const width = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
const height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
const displayWidth = width * 0.7;
const displayHeight = height * 0.8;
const centerX = width * 0.5;
const centerY = height * 0.5;
const startX = centerX - displayWidth/2;
const startY = centerY - displayHeight/2;

const background = () => {
  const display = two.makeRectangle(centerX, centerY, displayWidth, displayHeight);
  display.fill = 'rgb(0, 200, 255)';
  display.opacity = 0.75;
  display.noStroke();
};

const grid = (rows, cols, cellWidth, cellHeight) => {
  const verticals = [];
  const horisontals = [];
  for(let i=0; i<rows; i++) {
    const y = startY + i * cellHeight;
    const row = two.makeLine(startX, y, startX + displayWidth, y);
    row.linewidth = 3;
    row.stroke = 'rgb(44, 180, 9)';
    horisontals.push(row);
  }
  for(let j=0; j<cols; j++) {
    const x = startX + j * cellWidth;
    const col = two.makeLine(x, startY, x, startY + displayHeight);
    col.linewidth = 3;
    col.stroke = 'rgb(255, 255, 9)';
    verticals.push(col);
  }
  const rowLines = two.makeGroup(verticals);
  const colLines = two.makeGroup(horisontals);
  two.makeGroup(rowLines, colLines);
};

const blocks = (terrain, cellHeight, cellWidth) => {
  const xOffset = cellWidth/2;
  const yOffset = cellHeight/2;
  const solids = [];
  for(let row=0; row<terrain.length; row++) {
    for(let col=0; col<terrain[row].length; col++) {
      const isBlock = terrain[row][col];
      if (isBlock) {
        const x = startX + col * cellWidth + xOffset;
        const y = startY + row * cellHeight + yOffset;
        const display = two.makeRectangle(x, y, cellWidth, cellHeight);
        display.fill = 'rgb(200, 30, 10)';
        display.opacity = 0.75;
        display.noStroke();
      }
    }
  }
  two.makeGroup(solids);
};

const draw = ({ enemies, player, terrain }) => {
  const rows = terrain.length;
  const cols = terrain[0].length;
  const cellWidth = displayWidth/cols;
  const cellHeight = displayHeight/rows;
  background();
  grid(rows, cols, cellWidth, cellHeight);
  blocks(terrain, cellHeight, cellWidth); 
  two.update();
};

class App {
  run() {
    fetch("http://localhost:3000/init")
      .then(data => {
        return data.json();
      })
      .then(res => {
        draw(res);
        if (!window.EventSource)
          alert("You're browser does not support EventSource needed for this page");

        const eventSource = new EventSource("/start");

        eventSource.addEventListener('data', (e) => {
          const data = JSON.parse(e.data);
          console.log(data);
        });

        eventSource.addEventListener("gameover", function(e) {
          console.log("gameover");
          eventSource.close();
        });
      });


    //if (!window.EventSource)
      //alert("You're browser does not support EventSource needed for this page");

    //const eventSource = new EventSource("/start");

    //eventSource.addEventListener('data', draw);
    //const data = JSON.parse(e.data);

    //eventSource.addEventListener("gameover", function(e) {
      //console.log("gameover");
    //});

    //const data = {
      //enemies: [
        //[2,2],
        //[2,10]
      //],
      //player: {
        //dir: "north",
        //fuel: 3,
        //pos: [3,3]
      //},
      //terrain: [
        //[true, false, true, false, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
        //[false, false, false, true, false, false, false, false, false, false],
        //[false, false, true, true, false, false, false, false, false, false],
        //[false, false, false, true, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
        //[false, false, false, false, false, false, false, false, false, false],
      //]
    //};
    
    //draw(data);
  }
}

export default App;
