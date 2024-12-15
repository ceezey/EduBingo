from flask import (
    Blueprint, render_template, redirect, url_for
)

main = Blueprint('main', __name__)

@main.route('/')
def home():
    '''
    redirect user to index.html ie sign-in page
    '''
    return redirect(url_for('main.login'))

@main.route('/login.html')
def login():
    '''
    renders login page
    '''
    return render_template('login.html')

@main.route('/dashboard.html')
def dashboard():
    '''
    renders dashboard page
    '''
    return render_template('dashboard.html')

@main.route('/otp.html')
def otp():
    '''
    renders otp page
    '''
    return render_template('otp.html')

@main.route('/signup.html')
def signup():
    '''
    renders signup page
    '''
    return render_template('signup.html')

@main.route('/forgot-password.html')
def forgotpassword():
    '''
    renders forgotpassword page
    '''
    return render_template('forgot-password.html')

@main.route('/success-otp.html')
def successotp():
    '''
    renders successotp page
    '''
    return render_template('success-otp.html')