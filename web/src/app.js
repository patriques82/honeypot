import Two from 'two.js';
import rock from './images/rock.jpeg';
import grass from './images/grass.jpeg';
import drone from './images/drone.png';
import tank from './images/tank.png';
    
const width = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
const height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
const displayWidth = width * 0.6;
const displayHeight = height * 0.8;

const elem = document.getElementById('root');
const two = new Two({ 
  fullscreen: true,
  types: Two.Types.svg
}).appendTo(elem);

const centerX = width * 0.5;
const centerY = height * 0.5;
const startX = centerX - displayWidth/2;
const startY = centerY - displayHeight/2;

const grid = (rows, cols, cellWidth, cellHeight) => {
  for(let i=0; i<=rows; i++) {
    const y = startY + i * cellHeight;
    const row = two.makeLine(startX, y, startX + displayWidth, y);
    row.linewidth = 1;
    row.opacity = 0.1;
    row.stroke = 'rgb(30, 30, 30)';
  }
  for(let j=0; j<=cols; j++) {
    const x = startX + j * cellWidth;
    const col = two.makeLine(x, startY, x, startY + displayHeight);
    col.linewidth = 1;
    col.opacity = 0.1;
    col.stroke = 'rgb(30, 30, 30)';
  }
};

const drawBlocks = (terrain, cellHeight, cellWidth, xOffset, yOffset) => {
  for(let row=0; row<terrain.length; row++) {
    for(let col=0; col<terrain[row].length; col++) {
      const x = startX + col * cellWidth + xOffset;
      const y = startY + row * cellHeight + yOffset;
      const display = two.makeRectangle(x, y, cellWidth, cellHeight);
      const isBlock = terrain[row][col];
      if (isBlock) {
        display.fill = two.makeTexture(rock);
      } else {
        display.fill = two.makeTexture(grass);
      }
      display.opacity = 0.75;
      display.noStroke();
    }
  }
};

const drawStats = (fuel) => {
  const marginHeight = height - displayHeight;
  const y = marginHeight/4;
  const text = two.makeText("Fuel: " + fuel, centerX, y);
  if (fuel < 10) {
    text.stroke = 'rgb(200, 30, 30)';
  }
  text.size = 30;
  text.linewidth = 2;
  text.visible = true;
}

const drawEntity = (pos, dir, imgPath, cellHeight, cellWidth, xOffset, yOffset) => {
  const col = pos[1]-1;
  const row = pos[0]-1;
  const x = startX + col * cellWidth + xOffset;
  const y = startY + row * cellHeight + yOffset;
  const sprite = two.makeSprite(imgPath, x, y);
  sprite.scale = cellHeight/sprite.height;
  switch(dir) {
    case "south":
      sprite.rotation += Math.PI;
      break;
    case "east":
      sprite.rotation -= Math.PI/2;
      break;
    case "west":
      sprite.rotation += Math.PI/2;
      break;
    default:
      break;
  }
}

const drawPlayer = ({ dir, fuel, pos }, cellHeight, cellWidth, xOffset, yOffset) => {
  drawEntity(pos, dir, tank, cellHeight, cellWidth, xOffset, yOffset);
  drawStats(fuel);
};

const drawEnemies = (enemies, cellHeight, cellWidth, xOffset, yOffset) => {
  enemies.forEach(({ dir, pos }) => {
    drawEntity(pos, dir, drone, cellHeight, cellWidth, xOffset, yOffset);
  });
};

const draw = ({ enemies, player, terrain }) => {
  const rows = terrain.length;
  const cols = terrain[0].length;
  const cellWidth = displayWidth/cols;
  const cellHeight = displayHeight/rows;
  const xOffset = cellWidth/2;
  const yOffset = cellHeight/2;
  //grid(rows, cols, cellWidth, cellHeight);
  drawBlocks(terrain, cellHeight, cellWidth, xOffset, yOffset); 
  drawPlayer(player, cellHeight, cellWidth, xOffset, yOffset);
  drawEnemies(enemies, cellHeight, cellWidth, xOffset, yOffset);
};

const gameOver = (status) => {
  const text = two.makeText("Game Over", centerX, centerY);
  text.size = 50;
  text.linewidth = 3;
  text.visible = true;
  const subtext = two.makeText(status, centerX, centerY + 40);
  subtext.size = 30;
  subtext.linewidth = 3;
  subtext.visible = true;
}

class App {
  async run() {
    const resp = await fetch("http://localhost:3000/init")
    const data = await resp.json();
    draw(data);
    two.update();

    if (!window.EventSource)
      alert("You're browser does not support EventSource");

    const eventSource = new EventSource("/start");
    eventSource.addEventListener('data', (e) => {
      two.clear();
      const data = JSON.parse(e.data);
      console.log(data);
      draw(data.env);
      two.update();
    });

    eventSource.addEventListener("gameover", (e) => {
      eventSource.close();
      two.clear();
      const data = JSON.parse(e.data);
      console.log(data);
      draw(data.env);
      gameOver(data.status);
      two.update();
    });
  }
}

export default App;
