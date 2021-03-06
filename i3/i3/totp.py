#!/usr/bin/python3

import hmac, base64, struct, hashlib, time, sys, os

def get_hotp_token(secret, intervals_no):
    try:
        key = base64.b32decode(secret, True)
        msg = struct.pack(">Q", intervals_no)
        h = hmac.new(key, msg, hashlib.sha1).digest()
        o = h[19] & 15
        h = (struct.unpack(">I", h[o:o+4])[0] & 0x7fffffff) % 1000000
        return (True, "{:06}".format(h))
    except:
        return (False, 0)

def get_totp_token(secret):
    return get_hotp_token(secret, intervals_no=int(time.time())//30)

def usage():
    print("usage: totp.py <name> <totp secret> [1|3]")
    print("or     totp.py --secret <totp secret>")
    print("or     totp.py --file <path to secret file>")
    exit(0)

if __name__ == "__main__":
    if len(sys.argv) == 1 or "-h" in sys.argv or "--help" in sys.argv:
        usage();

    if len(sys.argv) == 3:
        secret = None
        if sys.argv[1] == "--secret":
            secret = sys.argv[2]
        elif sys.argv[1] == "--file":
            with open(sys.argv[2], "rb") as f:
                secret = f.readline().strip()
        if secret is not None:
            succ, token = get_totp_token(secret)
            if succ:
                print(token)
            exit(0)


    if len(sys.argv) > 3 and sys.argv[3] == "1":
        token = ""
        succ = False
        try:
            succ, token = get_totp_token(sys.argv[2])
        except:
            token = "<error>"
            pass
        if not succ:
            output = "{}:------ ".format(sys.argv[1])
            print(output)
            print(output)
            print("#ff0000")
        else:
            output = "{}:{} ".format(sys.argv[1], token)
            tmpfile = "/tmp/{}".format(sys.argv[1])
            print(output)
            print(output)
            print("#00ff00")
    elif len(sys.argv) > 1:
        output = "{}:------ ".format(sys.argv[1])
        print(output)
        print(output)
    else:
        usage()
