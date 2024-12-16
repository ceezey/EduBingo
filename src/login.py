# from functools import wraps
from . import CREDENTIALS_FILE
from flask import (
    Blueprint, jsonify, request,
    session
)

user = Blueprint('user', __name__)

# def login_required(function):
#     '''
#     function wrapper that checks login status
#     '''
#     @wraps(function)
#     def wrap(*args, **kwargs):
#         user_info = session.get('user_info', None)
#         if user_info is None:
#             # not logged in
#             flash("Please login first")
#             return redirect(url_for("main.index"))
#         else:
#             # logged in
#             return function(*args, **kwargs)
#     return wrap

@user.route("/login", methods=['POST'])
def login():
    try:
        # Get email and password from the POST request
        data = request.get_json() # Retrieve JSON data
        email = data.get("email")
        password = data.get("password")
        print(f"Email:", {email})
        print(f"Password:", {password})

        b = validate_login(email, password) # Verify user credentials
        if b == True:
            return jsonify({"status": "success", "message": "Login Success! Redirecting to dashboard."}), 201
        else:
            return jsonify({"status": "error", "message": "Account doesn't exist."}), 409
        
    except Exception as e:
        # Return any server error
        return jsonify({"status": "error", "message": f"An error occurred: {str(e)}"}), 500
        

# Route: Registration Page
@user.route('/signup', methods=['POST'])
def signup():
    try:
        # Get email and password from the POST request
        data = request.get_json() # Retrieve JSON data
        email = data.get("email")
        password = data.get("password")
        print(f"Email:", {email})
        print(f"Password:", {password})
        
        a = email_exists(email) # Validate unique user
        if a == True:
            return jsonify({"status": "error", "message": "Email already exists."}), 409

        # Register the user
        register_user(email, password)
        return jsonify({"status": "success", "message": "Account successfully created!"}), 201 
    
    except Exception as e:
        # Return any server error
        return jsonify({"status": "error", "message": f"An error occurred: {str(e)}"}), 500

@user.route("/signout", methods=['POST'])
# @login_required
def signout():
    try:
        # Clear the session or mimic logout logic
        email = request.form.get('email')  # The email sent with the request
        print(f"Logging out user: {email}")  # Debugging

        # Here you can invalidate session information (if using sessions)
        session.pop('user_info', None)  # Remove user info from session

        print(f"{email} has logged out.\n")

        return jsonify({"status": "success", "message": "Successfully logged out."}), 200
    except Exception as e:
        return jsonify({"status": "error", "message": str(e)}), 500

# Helper Function: Validate Login Credentials
def validate_login(email, password):
    try:
        with open(CREDENTIALS_FILE, "r") as file:
            for line in file:
                # Split the line into email and password
                parts = line.strip().split(",", 1)  # Split on the first comma only
                if len(parts) != 2:
                    continue  # Skip invalid lines
                
                stored_email = parts[0].strip()  # Remove unnecessary spaces
                stored_password = parts[1].strip()  # Remove unnecessary spaces
                
                print(f"Stored email: {stored_email}")
                print(f"Stored password: {stored_password}")

                # Check if credentials match
                if stored_email == email and stored_password == password:
                    return True
    except FileNotFoundError:
        pass
    return False  # Default to False if no match is found

# Helper Function: Signup User
def register_user(email, password):
    with open(CREDENTIALS_FILE, 'a') as file:
        file.write(f"{email}, {password}\n")
        print(f"Successfully registered: {email}")
        return True

# Helper Function: Validate Unique Account    
def email_exists(email):

    print(f"Received email: {email}")  # Debugging log to see the email input

    # """Check if the email already exists in the credentials file."""
    try:
        with open(CREDENTIALS_FILE, "r") as file:
            for line in file:
                # Split by the tab character since emails and passwords are separated by tabs
                stored_email = line.strip().split(",", 1)[0]  # Split on "," and ignore password
                print(f"Stored email: {stored_email}")  # Debugging log to see the email input
                if stored_email == email:
                    print(f"Email already exists")
                    return True  # Email exists
        return False  # Email not found
    except FileNotFoundError:
        return False  # If file doesn't exist, email can't exist either