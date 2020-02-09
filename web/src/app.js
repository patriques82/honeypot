import Two from 'two.js';
    
const elem = document.getElementById('root');
const width = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
const height = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);

const displayWidth = width * 0.6;
const displayHeight = height * 0.8;

const two = new Two({ 
  fullscreen: true,
  types: Two.Types.svg
}).appendTo(elem);

const centerX = width * 0.5;
const centerY = height * 0.5;
const startX = centerX - displayWidth/2;
const startY = centerY - displayHeight/2;

const background = (centerX, centerY, displayWidth, displayHeight) => {
  const display = two.makeRectangle(centerX, centerY, displayWidth, displayHeight);
  display.fill = 'rgb(0, 200, 255)';
  display.opacity = 0.75;
  display.noStroke();
};

const grid = (rows, cols, cellWidth, cellHeight) => {
  background();
  for(let i=0; i<rows; i++) {
    const y = startY + i * cellHeight;
    const row = two.makeLine(startX, y, startX + displayWidth, y);
    row.linewidth = 3;
    row.stroke = 'rgb(44, 180, 9)';
  }
  for(let j=0; j<cols; j++) {
    const x = startX + j * cellWidth;
    const col = two.makeLine(x, startY, x, startY + displayHeight);
    col.linewidth = 3;
    col.stroke = 'rgb(255, 255, 9)';
  }
};

const drawBlocks = (terrain, cellHeight, cellWidth, xOffset, yOffset) => {
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
};

const entity = (x, y, cellHeight, cellWidth, imgPath, dir) => {
  const display = two.makeRectangle(x, y, cellWidth, cellHeight);
  display.fill = 'rgb(150, 10, 100)';
  display.opacity = 0.75;
  display.noStroke();
  const sprite = two.makeSprite(imgPath, x, y);
  sprite.scale = sprite.height/cellHeight;
  return sprite;
}

const drawPlayer = ({ dir, fuel, pos }, cellHeight, cellWidth, xOffset, yOffset) => {
  const col = pos[1]-1;
  const row = pos[0]-1;
  const x = startX + col * cellWidth + xOffset;
  const y = startY + row * cellHeight + yOffset;
  const player = entity(x, y, cellHeight, cellWidth, 'tank.png', dir);
  console.log(dir, fuel, pos);
  switch(dir) {
    case "south":
      player.rotation += Math.PI;
      break;
    case "east":
      player.rotation -= Math.PI/2;
      break;
    case "west":
      player.rotation += Math.PI/2;
      break;
    default:
      break;
  }
};

const drawEnemies = (enemies, cellHeight, cellWidth, xOffset, yOffset) => {
  enemies.forEach(({ pos }) => {
    const col = pos[1]-1;
    const row = pos[0]-1;
    const x = startX + col * cellWidth + xOffset;
    const y = startY + row * cellHeight + yOffset;
    entity(x, y, cellHeight, cellWidth, 'enemy2.png', null);
  });
};

const draw = ({ enemies, player, terrain }) => {
  const rows = terrain.length;
  const cols = terrain[0].length;
  const cellWidth = displayWidth/cols;
  const cellHeight = displayHeight/rows;
  const xOffset = cellWidth/2;
  const yOffset = cellHeight/2;
  background(centerX, centerY, displayWidth, displayHeight);
  grid(rows, cols, cellWidth, cellHeight);

  drawBlocks(terrain, cellHeight, cellWidth, xOffset, yOffset); 
  drawPlayer(player, cellHeight, cellWidth, xOffset, yOffset);
  drawEnemies(enemies, cellHeight, cellWidth, xOffset, yOffset);
};

const gameOver = (data) => {
  draw(data);
  const text = two.makeText("Game Over", centerX, centerY);
  text.size = 30;
  text.linewidth = 2;
  text.visible = true;
}

const test = () => {
  const texture = new Two.Texture('tank.png');
  grid(rows, cols, cellWidth, cellHeight);
  
}

class App {
  async run() {
    const data = await fetch("http://localhost:3000/init")
    const json = data.json();

    if (!window.EventSource)
      alert("You're browser does not support EventSource");

    const eventSource = new EventSource("/start");
    eventSource.addEventListener('data', (e) => {
      two.clear();
      const data = JSON.parse(e.data);
      draw(data);
      two.update();
    });

    eventSource.addEventListener("gameover", (e) => {
      eventSource.close();
      two.clear();
      const data = JSON.parse(e.data);
      gameOver(data);
      two.update();
    });
  }
}

export default App;
