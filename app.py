from src import init

app = init()

if __name__ == '__main__':
    app.run('0.0.0.0', threaded=False)
