from flask import Flask, request, jsonify, redirect, url_for
import random
import smtplib
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from src import init

app = init()

@app.route('/send-otp', methods=['POST'])
def send_otp():
    email = request.form['email']
    otp = random.randint(100000, 999999)
    
    # Email sending logic
    sender_email = "your-email@example.com"
    sender_password = "your-password"
    subject = "Your OTP Code"
    body = f"Your OTP code is {otp}"
    
    msg = MIMEMultipart()
    msg['From'] = sender_email
    msg['To'] = email
    msg['Subject'] = subject
    msg.attach(MIMEText(body, 'plain'))
    
    try:
        smtp_server = smtplib.SMTP('smtp.example.com', 587)
        smtp_server.starttls()
        smtp_server.login(sender_email, sender_password)
        smtp_server.sendmail(sender_email, email, msg.as_string())
        smtp_server.quit()
        return redirect(url_for('otp'))
    except Exception as e:
        return jsonify({'message': 'Failed to send OTP', 'error': str(e)}), 500

@app.route('/otp')
def otp():
    return "OTP has been sent to your email address."

if __name__ == '__main__':
    app.run('0.0.0.0', threaded=False)
