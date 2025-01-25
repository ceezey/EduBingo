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

document.addEventListener("DOMContentLoaded", function () {
    // Get the email form (on forgot password page)
    const emailForm = document.getElementById("get-email-form");
  
    if (emailForm) {
      emailForm.addEventListener("submit", function (event) {
        event.preventDefault();
  
        const emailInput = document.getElementById("email");
        const email = emailInput.value.trim(); // Trim to avoid leading/trailing spaces

        if (email) {
            console.log("Email captured:", email);
          // Save the email to localStorage
          localStorage.setItem("userEmail", email);
  
          // Redirect to the password change page (otp.html)
          window.location.href = "/otp.html";
        } else {
          alert("Please enter a valid email.");
        }
      });
    } else {
        console.error("Email form not found on the page.");
    }
  
    // Get the password change form (on change password page)
    const passwordForm = document.getElementById("change-password-form");
  
    if (passwordForm) {
      passwordForm.addEventListener("submit", async function (event) {
        event.preventDefault();
  
        const password = document.getElementById("password").value;
        const confirmPassword = document.getElementById("confirm-password").value;
        const email = localStorage.getItem("userEmail");
        const successMessage = document.getElementById("success-message");
        const errorMessage = document.getElementById("error-message");

        console.log(email, password);
        if (!email) {
          alert("Email not found. Redirecting...");
          window.location.href = "/forgot-password.html"; // Redirect back if email is missing
          return;
        }
  
        // Validate that passwords match
        if (password !== confirmPassword) {
          errorMessage.textContent = "Passwords do not match.";
          errorMessage.style.display = "block";
          successMessage.style.display = 'none';
          return;
        }
  
        try {
          // Make a PUT request to the backend
          const response = await fetch("/change_pass", {
            method: "PUT",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({
              email: email,
              new_pass: password,
            }),
          });
  
          const data = await response.json();
  
          // Handle the response
          if (response.ok) {
            errorMessage.style.display = "none";
            successMessage.textContent = "Password changed successfully. Redirecting to login...";
            successMessage.style.display = "block";
  
            // Redirect after success
            setTimeout(() => {
              window.location.href = "/login.html";
            }, 2000);
          } else {
            // Display backend error message
            
            errorMessage.textContent = data.message || "An error occurred.";
            errorMessage.style.display = "block";
          }
        } catch (error) {
          // Handle network or unexpected errors
          errorMessage.textContent = "An error occurred. Please try again later.";
          errorMessage.style.display = "block";
        }
      });
    }
  });
  