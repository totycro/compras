#!/usr/bin/env python3

import json
import socketserver
import http.server


class Handler(http.server.SimpleHTTPRequestHandler):
    def send_all_good(self):
        self.send_response(200)
        self.send_header('Content-type', 'application/json')
        self.end_headers()

    def do_GET(self):
        self.send_all_good()
        self.wfile.write(
            json.dumps(
                [
                    {
                        'name': "Zeugs 1",
                        'items':[
                            {
                                'id': 432,
                                'name': 'Gurke',
                                'addedBy': {
                                'name': 'Moni',
                                },
                                'buyAt': [],
                                'bought': False,
                            },
                            {
                                'id': 947,
                                'name': 'Gurke2',
                                'addedBy': {
                                'name': 'Moni',
                                },
                                # 'buyAt': [ 'Hofer', 'Billa' ],
                                'buyAt': [],
                                'bought': True,
                            },
                        ]
                    },
                    {
                        'name': "Zeugs 2",
                        'items': [],
                    }
                ],
            ).encode('utf-8') + b'\n'
        )


    def do_GET_elm(self):
        self.send_all_good()
        self.wfile.write(
            json.dumps(
                [
                    {
                        'name': 'A. Bar',
                        'role': 'Doer',
                    },
                    {
                        'name': 'B. Foo',
                        'role': 'Dentist',
                    },
                ]
            ).encode('utf-8') + b'\n'
        )

    def do_PUT(self):
        self.send_all_good()
        self.wfile.write(
            json.dumps(
                {
                    'name': 'A. Bar',
                    'role': 'Doer',
                },
            ).encode('utf-8') + b'\n'
        )

    def do_PATCH(self):
        self.send_all_good()
        length = int(self.headers['Content-Length'])
        content_str = self.rfile.read(length)
        content = json.loads(content_str)
        self.wfile.write(
            json.dumps(
                {
                    'bought': content['bought']
                },
            ).encode('utf-8') + b'\n'
        )






port = 8008
print(f'Server listening on port {port}...')
socketserver.TCPServer.allow_reuse_address = True
httpd = socketserver.TCPServer(('127.0.0.1', port), Handler)
httpd.serve_forever()


