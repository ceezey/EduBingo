import os
from flask import Flask

# File to store user credentials
CREDENTIALS_FILE = os.path.join(os.path.dirname(__file__), '../ACCOUNT-FILE.dat')
TASK_FILE = os.path.join(os.path.dirname(__file__), '../TASK-FILE.dat')

app = Flask(__name__, template_folder='../templates', static_folder='../static')
app.config['SECRET_KEY'] = os.urandom(99)

def init():
    from .login import user # Adjust the import path according to your project structure
    app.register_blueprint(user)

    from .views import main  # Adjust the import path according to your project structure
    app.register_blueprint(main, url_prefix="/")

    from .dashboard import nya # Adjust the import path according to your project structure
    app.register_blueprint(nya)

    return app

@app.errorhandler(Exception)
def all_exception_handler(error):
    print("**all_exception_handler**")
    print(error)
    return "invalid", 500