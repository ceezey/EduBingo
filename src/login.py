from . import CREDENTIALS_FILE
from functools import wraps
from flask import (
    Blueprint, jsonify, request,
    url_for, redirect, session, flash
)

user = Blueprint('user', __name__)

def login_required(function):
    '''
    function wrapper that checks login status
    '''
    @wraps(function)
    def wrap(*args, **kwargs):
        user_info = session.get('user_info', None)
        if user_info is None:
            # not logged in
            flash("Please login first")
            return redirect(url_for("main.index"))
        else:
            # logged in
            return function(*args, **kwargs)
    return wrap

@user.route("/login", methods=['POST'])
def login():
    if request.method == 'POST':
        email = request.form('email')
        password = request.form('password')
        
        # Verify user credentials
        if validate_login(email, password):
            return redirect(url_for('main.dashboard', email=email))
        else:
            return redirect(url_for('main.login', message="Invalid email or password."))

# Route: Registration Page
@user.route('/signup', methods=['POST'])
def signup():
    try:
        data = request.get_json()
        email = data("email")
        password = data("password")

        if not email or not password:
            return jsonify({"status": "error", "message": "Email and password are required."})
        
        if email_exists(email):
            return jsonify({"status": "error", "message": "Email already exists."})

        register_user(email, password)
        return jsonify({"status": "success", "message": "Account successfully created!"})
    except Exception as e:
        return jsonify({"status": "error", "message": str(e)})

@user.route("/signout", methods=['POST'])
@login_required
def signout():
    session('user_info') == None
    flash("Logged out")
    return redirect(url_for('main.login'))

@user.route('/register-account-check', methods=['POST'])
def register_account_check():
    email = request.form.get('email')
    print(f"Received email: {email}")  # Debugging log to see the email input
    
    if not email:
        return "Email is required"
    
    if email_exists(email):
        return "Email already exists."
    else:
        return "Email is available."


# Helper Function: Validate Login Credentials
def validate_login(email, password):
    try:
        with open(CREDENTIALS_FILE, "r") as file:
            for line in file:
                stored_email, stored_password = line.strip().split(",", 1)
                if stored_email == email and stored_password == password:
                    return True
    except FileNotFoundError:
        pass
    return False

# Helper Function: Signup User
def register_user(email, password):
    # Check if the user already exists
    try:
        # Check if file exists and read for duplicates
        try:
            with open(CREDENTIALS_FILE, 'r') as file:
                for line in file:
                    stored_email, _ = line.strip().split(",", 1)
                    if stored_email == email:
                        print(f"Email {email} already exists.")
                        return False  # Email already exists
        except FileNotFoundError:
            print("File not found. Creating a new file.")
            # If file doesn't exist, we proceed to create it

        # Append the new credentials to the file
        with open(CREDENTIALS_FILE, 'w') as file:
            file.write(f"{email},{password}\n")
            print(f"Successfully registered: {email}")
            return True

    except Exception as e:
        print(f"Error during registration: {e}")
        return False
    
def email_exists(email):
    # """Check if the email already exists in the credentials file."""
    try:
        with open(CREDENTIALS_FILE, "r") as file:
            for line in file:
                stored_email = line.strip().split("_", 1)[0]  # Extract email (before the _ delimiter)
                if stored_email == email:
                    return True  # Email exists
        return False  # Email not found
    except FileNotFoundError:
        return False  # If file doesn't exist, email can't exist either