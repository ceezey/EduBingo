document.addEventListener("DOMContentLoaded", () => {
    const taskContainer = document.getElementById("TaskContainer");
    const addTask = document.querySelector("addButton"); // Fix selector
    const textInput = document.getElementById("name"); // Task Name
    const dayInput = document.getElementById("due-date-day");
    const monthInput = document.getElementById("due-date-month");
    const yearInput = document.getElementById("due-date-year");
    const combinedDateInput = document.getElementById("combined-due-date");
    const taskDescription = document.getElementById("description"); // Task Description
    const toDo = document.getElementById("to-do-radio");
    const doing = document.getElementById("doing-radio");
    const done = document.getElementById("done-radio");

    // Function to generate a unique numeric ID
    const generateNumericID = () => {
        return Date.now() + Math.floor(Math.random() * 1000);
    };

    // Function to get and combine day, month, and year
    const getCombinedDate = () => {
        const day = dayInput.value.padStart(2, "0"); // Ensure two digits
        const month = monthInput.value.padStart(2, "0");
        const year = yearInput.value;

        if (year && month && day) {
            return `${year}-${month}-${day}`; // Return YYYY-MM-DD format
        } else {
            return ""; // Return empty if any field is missing
        }
    };

    // Function to determine the task status
    const getTaskStatus = () => {
        if (toDo.checked) return "To-Do";
        if (doing.checked) return "Doing";
        if (done.checked) return "Done";
        return "Unknown"; // Default status
    };

    // Function to handle data submission
    const saveData = () => {
        event.preventDefault(); // Prevent default form submission

        const taskName = textInput.value.trim(); // Task Name
        const description = taskDescription.value.trim(); // Task Description
        const combinedDate = getCombinedDate(); // Combined Due Date
        const status = getTaskStatus(); // Task Status

        // Validate input fields
        if (!taskName || !combinedDate || !status) {
            swal({
                title: "Error",
                text: "Please enter task name, due date, and select status!",
                icon: "error",
            });
            return;
        }

        // Generate a unique numeric ID
        const id = generateNumericID();

        // Create a new object representing the to-do task
        const task = {
            id: id,
            name: taskName,
            description: description,
            date: combinedDate,
            status: status,
            completed: status === "Done", // Auto-mark as completed if status is "Done"
            timestamp: Date.now(),
        };

        // Save the JSON data to LocalStorage
        localStorage.setItem(id, JSON.stringify(task));

        // // Clear the input fields after saving
        // textInput.value = "";
        // taskDescription.value = "";
        // dayInput.value = "";
        // monthInput.value = "";
        // yearInput.value = "";
        // toDo.checked = false;
        // doing.checked = false;
        // done.checked = false;

        // Display success message and refresh task list
        swal({
            title: "Success",
            text: "Task successfully added!",
            icon: "success",
        });

        // displayTasks(); // Call a function to display the tasks
    };

    if (addTask) {
        addTask.addEventListener("submit", saveData);
    } else {
        console.error("addTask element not found.");
    }

    // Event listener for the Add Task button
    addTask.addEventListener("submit", saveData);

    // Add keypress event listener to the text input and date input
    taskName.addEventListener("keypress", (event) => {
        if (event.key === "Enter") {
            saveData();
        }
    });

    combinedDateInput.addEventListener("keypress", (event) => {
        if (event.key === "Enter") {
            saveData();
        }
    });

    textInput.addEventListener("keypress", (event) => {
        if (event.key === "Enter") 
            saveData(event);
    });
    

        // taskContainer.innerHTML = filteredTasks
        //     .map(
        //         (task) => `
        //     <li class="task-item" id="${task.id}">
        //         <button class="task-button">
        //             <div>
        //                 <p class="task-name">${task.name}</p>
        //                 <p class="task-due-date">${task.date}</p>   
        //             </div>
        //             <!-- arrow -->
        //             <iconify-icon
        //                 icon="material-symbols:arrow-back-ios-rounded"
        //                 style="color: black"
        //                 width="18"
        //                 height="18"
        //                 class="arrow-icon"
        //             ></iconify-icon>
        //         </button>
        //     </li>
        // `
        //     )
        //     .join("");

    tasks.forEach((task) => {
        const taskItem = `
            <li class="task-item" id="${task.id}">
                <button class="task-button">
                    <div>
                        <p class="task-name">${task.name}</p>
                        <p class="task-due-date">${task.date}</p>   
                    </div>
                    <!-- arrow -->
                    <iconify-icon
                        icon="material-symbols:arrow-back-ios-rounded"
                        style="color: black"
                        width="18"
                        height="18"
                        class="arrow-icon"
                    ></iconify-icon>
                </button>
            </li>`;
        taskContainer.insertAdjacentHTML("beforeend", taskItem);
    });
    
    

    // Attach event listener to the parent container
    taskContainer.addEventListener("click", (event) => {
        // Handle checkbox change
        if (event.target.type === "checkbox" && event.target.name === "task") {
            const taskId = event.target.id;
            const task = tasks.find((task) => task.id.toString() === taskId);
            if (task) {
                task.completed = event.target.checked;
                localStorage.setItem(task.id, JSON.stringify(task));
                const marker = event.target.nextElementSibling;
                if (marker.classList.contains("marker")) {
                    marker.classList.toggle("done", task.completed);
                }
            }
        }

        // Handle delete icon click
        if (event.target.classList.contains("bx-trash-alt")) {
            const taskId = event.target.closest(".card").dataset.taskId;
            swal({
                title: "Delete current task?",
                text: "Once deleted, you will not be able to recover this task!",
                icon: "warning",
                buttons: true,
                dangerMode: true,
            }).then((willDelete) => {
                if (willDelete) {
                    localStorage.removeItem(taskId);
                    event.target.closest(".card").remove();
                    swal("Poof! Your task has been deleted!", {
                        icon: "success",
                    });
                }
            });
        }

        if (event.target.classList.contains("date")) {
            const taskId = event.target.closest(".card").dataset.taskId;
            const task = tasks.find((task) => task.id.toString() === taskId);
    
            if (task) {
                // Trigger the date picker
                document.getElementById('hiddenDatePicker').showPicker();
    
                // Listen for changes in the date picker
                document.getElementById('hiddenDatePicker').addEventListener('change', function () {
                    const selectedDate = this.value;
    
                    // Ask for confirmation before updating the date
                    swal({
                        title: "Are you sure?",
                        text:  `Update the due date from ${task.date} to ${selectedDate}.`,
                        icon: "info",
                        buttons: ["Cancel", "Yes"],
                    }).then((willChangeDate) => {
                        if (willChangeDate) {
                            // Update the date in local storage
                            task.date = selectedDate;
                            localStorage.setItem(taskId, JSON.stringify(task));
    
                            // Refresh the display
                            displayTasks(currentSection);
                        } else {
                            // Reset the date picker if the user cancels
                            document.getElementById('hiddenDatePicker').value = '';
                        }
                    });
                });
            }
        }
    });
    showDivisionsWithDelay();
});

    