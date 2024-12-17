// elements
const radioViewOptions = document.querySelectorAll("input[name='view-option']");
const listView = document.getElementById("list-view");
const boardView = document.getElementById("board-view");
const addTaskCTA = document.getElementById("add-task-cta");
const setTaskOverlay = document.getElementById("set-task-overlay");
const closeButtons = document.querySelectorAll(".close-button");
const statusSelect = document.getElementById("status-select");
const statusDropdown = document.getElementById("status-dropdown");
const radioInputs = document.querySelectorAll(".radio-input");
const statusDisplay = statusSelect.querySelector("span");
const taskItems = document.querySelectorAll(".task-item");
const viewTaskOverlay = document.getElementById("view-task-overlay");
const deleteTaskCTA = document.getElementById("delete-task-cta");
const notification = document.getElementById("notification");
// the current active overlay
let activeOverlay = null;

//** event listeners **//

// radio buttons for view option
radioViewOptions.forEach((radioButton) => {
  radioButton.addEventListener("change", (event) => {
    const eventTarget = event.target;
    const viewOption = eventTarget.value;

    switch (viewOption) {
      case "list":
        boardView.classList.add("hide");
        listView.classList.remove("hide");
        break;
      case "board":
        listView.classList.add("hide");
        boardView.classList.remove("hide");
        break;
    }
  });
});

// add task
addTaskCTA.addEventListener("click", () => {
  setTaskOverlay.classList.remove("hide");
  activeOverlay = setTaskOverlay;
  // disable scrolling for content behind the overlay
  document.body.classList.add("overflow-hidden");
});

export function closeOverlay() {
  // close buttons inside overlays
  closeButtons.forEach((button) => {
    button.addEventListener("click", () => {
      activeOverlay.classList.add("hide");
      activeOverlay = null;
      // reenable scrolling
      document.body.classList.remove("overflow-hidden");
    });
  });
  if (closeButtons) {
    closeButtons.style.display = 'none';  // Make the overlay visible
  } else {
      console.error('Overlay not found');
  }
}

 // Function to toggle the dropdown visibility
 const toggleDropdown = () => {
  statusDropdown.classList.toggle("hide");
};

// Close the dropdown when clicking outside
const closeDropdown = (event) => {
  if (
    !statusSelect.contains(event.target) &&
    !statusDropdown.contains(event.target)
  ) {
    statusDropdown.classList.add("hide");
  }
};

// Update the displayed status when an option is clicked
const updateStatus = (event) => {
  const selectedValue = event.target.value;
  statusDisplay.textContent = selectedValue; // Update the display text
  toggleDropdown(); // Hide the dropdown
};

// Add event listeners
statusSelect.addEventListener("click", toggleDropdown);
document.addEventListener("click", closeDropdown);

radioInputs.forEach((input) => {
  input.addEventListener("change", updateStatus);
});

export function openOverlay() {
  // click a task
  console.log('Overlay function triggered')
  taskItems.forEach((task) => {
    task.addEventListener("click", () => {
      viewTaskOverlay.classList.remove("hide");
      activeOverlay = viewTaskOverlay;
      // disable scrolling for content behind the overlay
      document.body.classList.add("overflow-hidden");
    });
  });
  if (viewTaskOverlay) {
    viewTaskOverlay.style.display = 'block';  // Make the overlay visible
  } else {
      console.error('Overlay not found');
  }
}

// delete a task
deleteTaskCTA.addEventListener("click", () => {
  activeOverlay.classList.add("hide");
  activeOverlay = null;
  // reenable scrolling
  document.body.classList.remove("overflow-hidden");
  // show notification & hide it after a while
  notification.classList.add("show");
  setTimeout(() => {
    notification.classList.remove("show");
  }, 3000);
});