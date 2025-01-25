// Generate a mock board
const generateMockBoard = () => {
    const whitelist = Array.from({ length: 91 }, (_, i) => i + 1);
    const board = Array.from({ length: 5 }, () => []);
    for (let x = 0; x < 5; x++) {
      for (let y = 0; y < 5; y++) {
        const randIndex = Math.floor(Math.random() * whitelist.length);
        board[x].push(whitelist[randIndex]);
        whitelist.splice(randIndex, 1);
      }
    }
    board[2][2] = "X"; // Center "Free" space
    return board;
  };
  
  // Render the Bingo board
  const renderBoard = (board) => {
    const boardContainer = document.getElementById("bingo-board");
    boardContainer.innerHTML = ""; // Clear the board
    for (let x = 0; x < 5; x++) {
      for (let y = 0; y < 5; y++) {
        const cell = document.createElement("div");
        cell.className = "bingo-cell";
        cell.dataset.x = x;
        cell.dataset.y = y;
        cell.innerText = board[x][y] === "X" ? "X" : ""; // Show 'X' for free space
        boardContainer.appendChild(cell);
  
        // Add click event listener
        cell.addEventListener("click", () => {
          if (board[x][y] === "X") {
            board[x][y] = ""; // Unmark the cell
            cell.classList.remove("marked");
            cell.innerText = "";
          } else {
            board[x][y] = "X"; // Mark the cell
            cell.classList.add("marked");
            cell.innerText = "X";
          }
        });
      }
    }
  };
  
  // Check win condition
  const checkWin = (board) => {
    // Check rows
    for (let row of board) {
      if (row.every((cell) => cell === "X")) return true;
    }
  
    // Check columns
    for (let col = 0; col < 5; col++) {
      if (board.every((row) => row[col] === "X")) return true;
    }
  
    // Check diagonals
    if (board.every((row, idx) => row[idx] === "X")) return true;
    if (board.every((row, idx) => row[4 - idx] === "X")) return true;
  
    return false;
  };
  
  // Initialize the game
  const board = generateMockBoard();
  renderBoard(board);
  
  // Add event listener to the "Check Win" button
  document.getElementById("check-win").addEventListener("click", () => {
    const resultMessage = document.getElementById("result-message");
    if (checkWin(board)) {
      resultMessage.textContent = "BINGO! You win!";
      resultMessage.style.color = "#28a745";
    } else {
      resultMessage.textContent = "Not yet! Keep going.";
      resultMessage.style.color = "#d9534f";
    }
  });
  