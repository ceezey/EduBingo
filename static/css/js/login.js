// Wait for the DOM to fully load
// User login
document.addEventListener("DOMContentLoaded", function () {
    const loginForm = document.getElementById("Login");

    // Form submission event listener
    loginForm.addEventListener("submit", function (event) {
        event.preventDefault(); // Prevent the default form submission behavior

        // Retrieve email and password values
        const email = document.getElementById("email").value;
        const password = document.getElementById("password").value;

        console.log("Email:", email);
        console.log("Password:", password);

        // Make a POST API fetch request to the backend
        response = fetch("/login", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                email: email,
                password: password,
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
                alert("Login Success! Redirecting to dashboard.");
                window.location.href = "/dashboard.html"; // Redirect to main page
            } else if (data.status === "error") {
                alert(`Error: ${data.message}`); // Show error message
            }
        })
        .catch(error => {
            console.error("Error:", error);
            alert("Incorrect email or password");
        });
    });
});

// User logout
document.addEventListener("DOMContentLoaded", function () {
    const signoutBttn = document.getElementById("signout");
    signoutBttn.addEventListener("click", function () {

        fetch("/update_show_again", {
            method: "PUT",
        })
            .then((response) => response.json())
            .then((data) => {
                if (data.status === "success") {
                    console.log("showAgain set to true on logout");
                } else {
                    console.error("Error updating showAgain:", data.message);
                }
            })
            .catch((error) => {
                console.error("Error:", error);
            })

        // Retrieve email and password values
        const email = document.getElementById("email");

        fetch("/signout", {
            method: "POST",
            headers: {
                "Content-Type": "application/x-www-form-urlencoded",
            },
            body: JSON.stringify({
                email: email, // Send email data
            }),
        })
        .then(response => response.json())  // Parse the JSON response
        .then(data => {
            if (data.status === "success") {
                alert("Logged out successfully!");
                window.location.href = "/login.html"; // Redirect to login page
            } else {
                alert(`Error: ${data.message}`);
            }
        })
        .catch(error => {
            console.error("Error:", error);
            alert("An error occurred during logout.");
        });
    });
});

// User signup
document.addEventListener("DOMContentLoaded", function () {
    const signupForm = document.getElementById("Account");

    // Form submission event listener
    signupForm.addEventListener("submit", function (event) {
        event.preventDefault(); // Prevent the default form submission behavior

        // Retrieve email and password values
        const email = document.getElementById("email").value;
        const password = document.getElementById("password").value;

        console.log("Email:", email);
        console.log("Password:", password);

        // Make a POST API fetch request to the backend
        response = fetch("/signup", {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({
                email: email,
                password: password,
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
                alert("Account successfully created!");
                window.location.href = "/login.html"; // Redirect to homepage or login page
            } else if (data.status === "error") {
                alert(`Error: ${data.message}`); // Show error message
            }
        })
        .catch(error => {
            console.error("Error:", error);
            alert("Invalid credentials, please try again");
        });
    });
});
