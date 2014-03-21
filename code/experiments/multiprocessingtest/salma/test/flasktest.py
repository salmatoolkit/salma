from flask import Flask
app = Flask(__name__)


@app.route('/<username>')
def hello_world(username):
    if username:
        return "Hello %s!" % username
    else:
        return "Hello World!"

if __name__ == '__main__':
    app.debug = True
    app.run()