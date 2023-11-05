import socket
import threading
import sys


# server tcp socket
def handle_connections():
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.bind((ip_server, puerto_server))
    sock.listen()
    while True:
        connection, addr = sock.accept()
        threading.Thread(target=handle_client, args=(connection, addr)).start()


# objeto cliente que tiene ip y puerto del cliente
class Client:
    def __init__(self, ip):
        self.ip = ip
        self.port = 0


# leer proximo comando
def get_next_command(connection: socket.socket):
    data = bytearray()
    last_char = b''
    while last_char != b'\n':
        last_char = connection.recv(1)
        data.append(last_char[0])
    return data


# define acciones dependiendo del comando
def handle_client(connection: socket.socket, addr):
    state = 'created'
    client = Client(addr[0])
    while state != 'closed':
        try:
            data = get_next_command(connection)
        except:
            if state == 'connected':
                with lock:
                    clientes_activos.remove(client)
            break

        if state == 'created' and data.startswith(b'CONECTAR'):
            client.port = int(data.split(b' ')[1])
            with lock:
                clientes_activos.append(client)
            state = 'connected'
        elif state == 'connected':
            if data.startswith(b'DESCONECTAR'):
                with lock:
                    clientes_activos.remove(client)
                state = 'closed'
            elif data.startswith(b'INTERRUMPIR'):
                with lock:
                    clientes_activos.remove(client)
                state = 'paused'
        elif state == 'paused' and data.startswith(b'CONTINUAR'):
            with lock:
                clientes_activos.append(client)
            state = 'connected'

        connection.sendall(b'OK\n')

    connection.close()


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Use: python python server.py <ServerIP> <ServerPort>")
        exit(1)
    else:
        ip_origen = "127.0.0.1"
        puerto_origen = 65534

        ip_salida = "127.0.0.1"
        puerto_salida = 12345

        ip_server = sys.argv[1]
        puerto_server = int(sys.argv[2])

        clientes_activos = []
        lock = threading.Lock()

        send_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        send_sock.bind((ip_salida, puerto_salida))

        # inicio hilo que establece la conexion TCP con el cliente
        threading.Thread(target=handle_connections).start()

        receive_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        receive_sock.bind((ip_origen, puerto_origen))
        while True:
            data = receive_sock.recv(65535)
            with lock:
                for client in clientes_activos:
                    send_sock.sendto(data, (client.ip, client.port))