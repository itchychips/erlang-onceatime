I wanted to see if I could write a bad replacement for nmap.  I succeeded.  It scans all possible ports (1-65535).

Using nmap_server:profile, here are my timings on Windows 10 for testing all
ports:

    Testing 64 workers.
    Shutting down nmap server.
    Result: 32.092979 seconds
    Testing 128 workers.
    Shutting down nmap server.
    Result: 36.059852 seconds
    Testing 256 workers.
    Shutting down nmap server.
    Result: 25.543987 seconds
    Testing 512 workers.
    Shutting down nmap server.
    Result: 23.552204 seconds
    Testing 1024 workers.
    Shutting down nmap server.
    Result: 21.181952 seconds
    Testing 2048 workers.
    Result: 21.073203 seconds
    Shutting down nmap server.
    Testing 4096 workers.
    Shutting down nmap server.
    Result: 21.078425 seconds
    Testing 8192 workers.
    Shutting down nmap server.
    Result: 21.104537 seconds
    Testing 16384 workers.
    Shutting down nmap server.
    Result: 21.104435 seconds
    Testing 32768 workers.
    Shutting down nmap server.
    Result: 21.156249 seconds
    Testing 65535 workers.
    Shutting down nmap server.
    Result: 21.320396 second

For reference, I tested nmap on the same computer using `-Pn -sT -T5 -p 1-900
--max-retries 1`, and I got 19.43 seconds for just 900 ports.  I feel like I'm
not 1-to-1'ing the test, but the highly parallel erlang works much faster
somehow.

Something is afoot, but I'm too tired to debug it and probably won't be
motivated later to try it.
