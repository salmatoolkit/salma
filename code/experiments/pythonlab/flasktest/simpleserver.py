from flask import Flask
from flask import render_template, make_response

app = Flask(__name__)


@app.route("/")
def hello():
    return render_template("index.html")


@app.route("/hello")
def number():
    resp = make_response("Hello Wrld!", 200)
    resp.headers['Access-Control-Allow-Origin'] = '*'
    resp.headers['Access-Control-Allow-Methods'] = 'GET,PUT,POST,DELETE'
    resp.headers['Access-Control-AllowHeaders'] = 'Content-Type'
    return resp

if __name__ == "__main__":
    app.run()