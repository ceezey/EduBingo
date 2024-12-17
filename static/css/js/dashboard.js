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
    
            // Create a new list item for the task
            const listItem = document.createElement("li");
            listItem.classList.add("task-item");
            listItem.id = task.id;
    
            listItem.innerHTML = `
                <li class="task-item">
                    <button class="task-button">
                        <div>
                            <p class="task-name">${task.name}</p>
                            <p class="task-due-date">${task.date}</p>
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

    