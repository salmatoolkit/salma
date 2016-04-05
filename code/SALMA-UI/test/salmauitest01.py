from bottle import route, run, static_file


@route('/hello')
def hello():
    return "Hello World!"


@route('/static/<filepath:path>')
def server_static(filepath):
    return static_file(filepath, root='testresources/')

if __name__ == '__main__':
    run(host='localhost', port=8080, debug=True)
