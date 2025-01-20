import { openOverlay, closeOverlay } from "./main.js";
document.addEventListener("DOMContentLoaded", () => {
    const addForm = document.getElementById("add");

    // Form submission event listener
    addForm.addEventListener("submit", async function (event) {
        event.preventDefault();  // Prevent form submission
    
        const taskName = document.getElementById("name"); // Task Name
        const dayInput = document.getElementById("due-date-day");
        const monthInput = document.getElementById("due-date-month");
        const yearInput = document.getElementById("due-date-year");
        const taskDescription = document.getElementById("description"); // Task Description
        const generateNumericID = () => Date.now() + Math.floor(Math.random() * 1000);
    
        const getCombinedDate = () => {
            const day = dayInput.value.padStart(2, "0"); // Ensure two digits
            const month = monthInput.value.padStart(2, "0");
            const year = yearInput.value;
            return year && month && day ? `${year}-${month}-${day}` : "";  // Return empty if any field is missing
        };
    
        // Function to get the selected status
        const getTaskStatus = () => {
            const radios = document.getElementsByName("status-option");  // Get all radio buttons with name 'status-option'
            
            for (let radio of radios) {
                if (radio.checked) {
                    return radio.value;  // Return the value of the selected radio button
                }
            }
            
            return null; // Return null if no radio button is selected
        };

        // Make a POST API fetch request to the backend
        response = fetch("/addtask", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                id: generateNumericID(),
                name: taskName.value,
                description: taskDescription.value,
                date: getCombinedDate(),
                status: getTaskStatus(),
            }),
        })
        .then(response => {
            if (!response.ok) {
                // Handle non-200 responses
                throw new Error(`HTTP error! Status: ${response.status}`);
            }
            return response.json(); // Parse as JSON
        })
        .then(data => {
            if (data.status === "success") {
                alert("Task Successfully Added!!!.");
                window.location.href = "/dashboard.html"; // Redirect to homepage or login page
            } else if (data.status === "error") {
                alert(`Error: ${data.message}`); // Show error message
            }
        })
        .catch(error => {
            console.error("Error:", error);
            alert("An error occurred, please try again.");
        });
    });

    // Function to fetch tasks based on category
    const fetchTasks = () => {
        fetch("/tasks")
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error! Status: ${response.status}`);
                }
                return response.json(); // Parse as JSON
            })
            .then(data => {
                if (data.status === "success" && Array.isArray(data.tasks)) {
                    data.tasks.forEach(task => displayTasks(task)); // Display each task
                } else {
                    console.error("Error fetching tasks or no tasks available.");
                }
            })
            .catch(error => {
                console.error("Error:", error);
                alert("An error occurred while fetching tasks.");
            });
    };

    const displayTasks = (task) => {
        // Loop through both listView and boardView
        const listViewContainers = ["listView", "boardView"];
        listViewContainers.forEach((view) => {
            const containerID = statusMapping[view][task.status];
            const taskContainer = document.getElementById(containerID);
            console.log("Status Mapping for Task:", statusMapping);
            if (!taskContainer) {
                console.error(`Missing container for ${view}:`, task.status);
                return;
            }
                    
            // Close button for set task overlay
            const closeSetTaskButton = document.getElementById("close-button");
            closeSetTaskButton.addEventListener("click", () => {
                const setTaskOverlay = document.getElementById("set-task-overlay");
                setTaskOverlay.classList.add("hide");
            });

            // Create a new list item for the task
            const listItem = document.createElement("li");
            listItem.classList.add("task-item");
            listItem.id = task.id;

            // Format the task date
            const formattedDate = new Date(task.date).toLocaleDateString('en-US', {
                year: 'numeric',
                month: 'long',
                day: 'numeric'
            });
    
            listItem.innerHTML = `
                <li class="task-item">
                    <button class="task-button">
                        <div>
                            <p class="task-name">${task.name}</p>
                            <span class="task-due-date">${formattedDate}</span>
                        </div>
                        <iconify-icon
                            icon="material-symbols:arrow-back-ios-rounded"
                            style="color: black;"
                            width="18"
                            height="18"
                            class="arrow-icon"
                        ></iconify-icon>
                    </button>
                </li>
            `;

            // Append to the corresponding container
            taskContainer.appendChild(listItem);

             // Add click event listener to the arrow icon
            const arrowIcon = listItem.querySelector(".arrow-icon");
            arrowIcon.addEventListener("click", (event) => {
                event.stopPropagation(); // Prevent parent button click
                console.log(`Arrow clicked for task: ${task.name}`);
                // Add further actions here, like opening details or navigating
                openOverlay();

                // Create the task details HTML
                const taskDetailsHTML = `
                    <h1 class="header no-margin">Name</h1>
                    <p id="task-name" class="value">${task.name}</p>
                    <h1 class="header">Description</h1>
                    <p id="task-description" class="value">${task.description}</p>
                    <div class="flex items-center">
                        <h1 class="header min-width">Due date</h1>
                        <p class="value">${formattedDate}</p>
                    </div>
                    <div class="flex items-center">
                        <h1 class="header min-width">Status</h1>
                        <p class="value status-value">
                            <span class="circle pink-background"></span><span>${task.status}</span>
                        </p>
                    </div>
                `;

                // Insert the task details HTML into the tasks-details div
                const tasksDetailsDiv = document.getElementById("tasks-details");
                tasksDetailsDiv.innerHTML = taskDetailsHTML;

                // Add event listener to delete button
                const deleteTaskButton = document.getElementById("delete-task-cta");
                deleteTaskButton.addEventListener("click", () => {
                    listItem.remove(); // Remove the corresponding li element
                });
            });
        });
    };

    const statusMapping = {
        listView: {
            "To do": "todo-task-list",      // IDs for List View
            "Doing": "doing-task-list",
            "Done": "done-task-list",
        },
        boardView: {
            "To do": "todo-task-board",  // IDs for Board View
            "Doing": "doing-task-board",
            "Done": "done-task-board",
        },
    };

    const changeStatus = (taskid, newStatus) => {
        fetch('/change_status', {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({
                taskid: taskid,
                status: newStatus
            }),
        })
        .then(response => response.json())
        .then(data => {
            if (data.status === 'success') {
                alert('Task status updated successfully!');
                // Optionally, update the UI with the new status
                const taskElement = document.getElementById(taskid);
                if (taskElement) {
                    const statusElement = taskElement.querySelector('.task-status');
                    if (statusElement) {
                        statusElement.textContent = newStatus;
                    }
                }
            } else {
                alert(`Error: ${data.message}`);
            }
        })
        .catch(error => {
            console.error('Error:', error);
            alert('An error occurred while updating the task status.');
        });
    };    

    const deleteTask = (taskid) => {
        fetch('/delete_task', {
            method: 'DELETE',
            headers: {
                'Content-Type': 'application/json',
            },
            body: JSON.stringify({ taskid: taskid }),
        })
        .then(response => response.json())
        .then(data => {
            if (data.status === 'success') {
                alert('Task deleted successfully!');
                // Optionally, remove the task from the UI
                const taskElement = document.getElementById(taskid);
                if (taskElement) {
                    taskElement.remove();
                }
            } else {
                alert(`Error: ${data.message}`);
            }
        })
        .catch(error => {
            console.error('Error:', error);
            alert('An error occurred while deleting the task.');
        });
    };

    fetchTasks();
});

// MOOD TRACKER
document.addEventListener("DOMContentLoaded", () => {
    const modal = document.getElementById("how-are-you-modal");
    const closeModalButton = document.getElementById("close-modal");
    const form = document.getElementById("how-are-you-form");

    // Show the modal when the page loads
    modal.style.display = "grid";

    // Close the modal when the close button is clicked
    closeModalButton.addEventListener("click", () => {
      modal.style.display = "none";
    });

    // Handle form submission
    form.addEventListener("submit", (event) => {
      event.preventDefault();
      const userResponse = document.getElementById("user-response").value;
      fetch("/save_mood", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({ mood: userResponse }),
      })
        .then((response) => response.json())
        .then((data) => {
          if (data.status === "success") {
            alert("Response saved successfully!");
            modal.style.display = "none";
          } else {
            alert(`Error: ${data.message}`);
          }
        })
        .catch((error) => {
          console.error("Error:", error);
          alert("An error occurred, please try again.");
        });
    });
  });