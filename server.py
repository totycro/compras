#!/usr/bin/env python3

import json
import re
import socketserver
import http.server

def unique_id_sequence():
    i = 0
    while True:
        yield i
        i +=  1

unique_id_sequence= unique_id_sequence()  # don't try this at home


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
                        'id': next(unique_id_sequence),
                        'name': "cositas para la casa",
                        'items':[
                            {
                                'id': next(unique_id_sequence),
                                'name': 'pepinos 1',
                                'addedBy': {
                                'name': 'Moni',
                                },
                                'buyAt': [],
                                'bought': False,
                            },
                            {
                                'id': next(unique_id_sequence),
                                'name': 'pepinos 2',
                                'addedBy': {
                                'name': 'Moni',
                                },
                                # 'buyAt': [ 'Hofer', 'Billa' ],
                                'buyAt': [],
                                'bought': True,
                            },
                            {
                                'id': next(unique_id_sequence),
                                'name': 'calabacín',
                                'addedBy': {
                                'name': 'Moni',
                                },
                                # 'buyAt': [ 'Hofer', 'Billa' ],
                                'buyAt': [],
                                'bought': False,
                            },
                            {
                                'id': next(unique_id_sequence),
                                'name': 'dulcítas',
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
                        'id': next(unique_id_sequence),
                        'name': "cositas para comer",
                        'items': [],
                    }
                ],
            ).encode('utf-8') + b'\n'
        )


    def do_POST(self):
        self.send_all_good()
        # assume query to add list
        content = self.read_json_content()
        print ("Post to path", self.path)

        add_item_match = re.match(r".*/list/(?P<list_id>\d+)/$", self.path)
        if add_item_match:
            print("Adding new item")
            list_id = add_item_match.group("list_id")
            self.wfile.write(
                json.dumps(
                    {
                        'id': next(unique_id_sequence),
                        'name': content['name'],
                        'addedBy': {
                            'name': 'Moni',
                        },
                        'buyAt': [],
                        'bought': False,
                    }
                ).encode('utf-8') + b'\n'
            )
        else:
            print("Adding new list")
            self.wfile.write(
                json.dumps(
                    {
                        'id': next(unique_id_sequence),
                        'name': content['name'],
                        'items': [],
                    }
                ).encode('utf-8') + b'\n'
            )

    def do_PATCH(self):
        self.send_all_good()
        content = self.read_json_content()
        self.wfile.write(
            json.dumps(
                {
                    'bought': content['bought']
                },
            ).encode('utf-8') + b'\n'
        )

    def read_json_content(self):
        length = int(self.headers['Content-Length'])
        content_str = self.rfile.read(length)
        return json.loads(content_str)






port = 8008
print(f'Server listening on port {port}...')
socketserver.TCPServer.allow_reuse_address = True
httpd = socketserver.TCPServer(('127.0.0.1', port), Handler)
httpd.serve_forever()


