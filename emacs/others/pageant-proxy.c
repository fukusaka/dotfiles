/*
 * http://slashdot.jp/~doda/journal/456908
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <sys/param.h>
#include <windows.h>

#define AGENT_COPYDATA_ID 0x804e50ba
#define AGENT_MAX_MSGLEN  8192

#define SSH_AGENTPID_ENV_NAME "SSH_AGENT_PID"
#define SSH_AUTHSOCKET_ENV_NAME "SSH_AUTH_SOCK"

char socket_name[MAXPATHLEN];
char socket_dir[MAXPATHLEN];

unsigned long get_uint32(unsigned char *buff) {
	return ((unsigned long)buff[0] << 24) +
	       ((unsigned long)buff[1] << 16) +
	       ((unsigned long)buff[2] <<  8) +
	       ((unsigned long)buff[3]);
}

void set_uint32(unsigned char *buff, unsigned long v) {
	buff[0] = (unsigned char)(v >> 24);
	buff[1] = (unsigned char)(v >> 16);
	buff[2] = (unsigned char)(v >>  8);
	buff[3] = (unsigned char)v;
	return;
}

unsigned long agent_request(char *out, unsigned long out_size, char *in) {
	HWND hwnd;
	char mapname[25];
	HANDLE fmap = NULL;
	unsigned char *p = NULL;
	COPYDATASTRUCT cds;
	unsigned long len;
	unsigned long ret = 0;

	if (out_size < 5) {
		return 0;
	}
	if ((len = get_uint32(in)) > AGENT_MAX_MSGLEN - 4) {
		goto agent_error;
	}

	hwnd = FindWindow("Pageant", "Pageant");
	if (!hwnd) {
		goto agent_error;
	}

	sprintf(mapname, "PageantRequest%08x", (unsigned)GetCurrentThreadId());
	fmap = CreateFileMapping(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE,
				 0, AGENT_MAX_MSGLEN, mapname);
	if (!fmap) {
		goto agent_error;
	}

	if ((p = MapViewOfFile(fmap, FILE_MAP_WRITE, 0, 0, 0)) == NULL ) {
		goto agent_error;
	}

	cds.dwData = AGENT_COPYDATA_ID;
	cds.cbData = strlen(mapname) + 1;
	cds.lpData = mapname;

	memcpy(p, in, len + 4);

	if (SendMessage(hwnd, WM_COPYDATA, (WPARAM)NULL, (LPARAM)&cds) > 0) {
		len = get_uint32(p);
		if (out_size >= len + 4) {
			memcpy(out, p, len + 4);
			ret = len + 4;
		}
	}

agent_error:
	if (p) {
		UnmapViewOfFile(p);
	}
	if (fmap) {
		CloseHandle(fmap);
	}

	if (ret == 0) {
		set_uint32(out, 1);
		out[4] = 5; // SSH_AGENT_FAILURE
	}

	return ret;
}

/*ARGSUSED*/
static void
cleanup_handler(int sig)
{
	if (socket_name[0])
		unlink(socket_name);
	if (socket_dir[0])
		rmdir(socket_dir);
	exit(2);
}

int main(void) {
	int sock, asock;
	unsigned long readlen, reqlen, recvlen;
	struct sockaddr_un addr;
	char buff[AGENT_MAX_MSGLEN];
	char *format, *pidstr, *agentsocket = NULL;
	pid_t pid;
	char pidstrbuf[1 + 3 * sizeof pid];

	if (agentsocket == NULL) {
		strlcpy(socket_dir, "/tmp/ssh-XXXXXXXXXX", sizeof(socket_dir));
		if (mkdtemp(socket_dir) == NULL) {
			perror("mkdtemp: private socket dir");
			exit(1);
		}
		snprintf(socket_name, sizeof(socket_name), "%s/agent.%ld",
			 socket_dir, (long)getpid());
	} else {
		socket_dir[0] = '\0';
		strlcpy(socket_name, agentsocket, sizeof(socket_name));
	}

	if ((sock = socket(PF_UNIX, SOCK_STREAM, 0)) == 0) {
		fprintf(stderr, "socket failed\n");
		exit(0);
	}
	memset(&addr, 0, sizeof(addr));
	addr.sun_family = AF_UNIX;
	strlcpy(addr.sun_path, socket_name, sizeof(addr.sun_path));

	unlink(socket_name);

	if (bind(sock, (struct sockaddr *)&addr, sizeof(addr)) != 0) {
		fprintf(stderr, "bind failed\n");
		goto cleanup;
	}

	if (listen(sock, -1) != 0) {
		fprintf(stderr, "listen failed\n");
		goto cleanup;
	}

	pid = fork();
	if (pid == -1) {
		perror("fork");
		goto cleanup;
	}

	if (pid != 0) {
		close(sock);
		snprintf(pidstrbuf, sizeof pidstrbuf, "%ld", (long)pid);

		format = "%s=%s; export %s;\n";
		printf(format, SSH_AUTHSOCKET_ENV_NAME, socket_name,
		       SSH_AUTHSOCKET_ENV_NAME);
		printf(format, SSH_AGENTPID_ENV_NAME, pidstrbuf,
		       SSH_AGENTPID_ENV_NAME);
		printf("echo Agent pid %ld;\n", (long)pid);
		exit(0);
	}

	signal(SIGINT, SIG_IGN);
	signal(SIGPIPE, SIG_IGN);
	signal(SIGHUP, cleanup_handler);
	signal(SIGTERM, cleanup_handler);

	while ((asock = accept(sock, NULL, NULL)) > 0) {
		recvlen = 0;
		while ((readlen = read(asock, buff+recvlen, 4 - recvlen)) > 0) {
			recvlen += readlen;
			if (recvlen < 4) {
				continue;
			}
			recvlen = 0;
			reqlen = get_uint32(buff);
			while ((readlen = read(asock, buff+4+recvlen, reqlen-recvlen)) > 0) {
				recvlen += readlen;
				if (recvlen < reqlen) {
					continue;
				}
				if ((readlen = agent_request(buff, sizeof(buff), buff)) > 0) {
					write(asock, buff, readlen);
				}
				else {
					set_uint32(buff, 1);
					buff[4] = 5; // SSH_AGENT_FAILURE
					write(asock, buff, 5);
				}
			}
			recvlen = 0;
		}
		shutdown(asock, SHUT_RDWR);
		close(asock);
	}

cleanup:
	shutdown(sock, SHUT_RDWR);
	close(sock);

	if (socket_name[0])
		unlink(socket_name);
	if (socket_dir[0])
		rmdir(socket_dir);

	exit(0);
}

/*
 * Local Variables:
 * compile-command: "gcc -o pageant-proxy pageant-proxy.c"
 * End:
 */
