import sys
import socket
import subprocess
import os
import signal

def await_connect(client_socket: socket.socket, port_vlc):
    print("Comandos:\n  1)CONECTAR\n")
    command = input("Ingrese un comando: ")

    if command == "1":
        client_socket.send('CONECTAR {}\n'.format(port_vlc).encode())
        vlc_command = f"vlc rtp://127.0.0.1:{port_vlc} > /dev/null 2>&1"
        vlc_process = subprocess.Popen(vlc_command, shell=True, preexec_fn=os.setsid)
    else:
        print("Comando no valido")
        return

    print("Respuesta del server: " + client_socket.recv(1024).decode())

    return vlc_process


def await_command(client_socket: socket.socket):
    print("Comandos:\n  1)INTERRUMPIR\n  2)CONTINUAR\n  3)DESCONECTAR\n ")
    command = input("Ingrese un comando: ")

    if command == "1":
        client_socket.send(b'INTERRUMPIR\n')
    elif command == "2":
        client_socket.send(b'CONTINUAR\n')
    elif command == "3":
        client_socket.send(b'DESCONECTAR\n')
    else:
        print("Comando no valido")
        return

    print("Respuesta del server: " + client_socket.recv(1024).decode())

    if command == "3":
        return 2
    else:
        return 1


def conection_server(ip, port, port_vlc):
    client_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    client_socket.connect((ip, port))

    print(f"Conexion exitosa con el servidor")

    estado = 0
    vlc_process = None
    while estado != 2:
        try:
            if estado == 0:
                vlc_process = await_connect(client_socket, port_vlc)
                estado = 1
            else:
                estado = await_command(client_socket)
        except:
            print("Se perdio la conexion con el servidor")
            break

    client_socket.close()
    if vlc_process is not None:
        os.killpg(os.getpgid(vlc_process.pid), signal.SIGTERM)


if __name__ == "__main__":
    if len(sys.argv) != 4:
        print("Use: python cliente.py <ServerIP> <ServerPort> <PuertoVLC>")
        exit(1)
    else:
        server_ip = sys.argv[1]
        server_port = int(sys.argv[2])
        puerto_vlc = int(sys.argv[3])

        print(f"Conectando a server IP: {server_ip}")
        print(f"Server Port: {server_port}")
        print(f"Puerto VLC: {puerto_vlc}")
        conection_server(server_ip, server_port, puerto_vlc)
