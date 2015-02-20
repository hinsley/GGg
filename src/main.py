import sys
from conversion import encode, decode

if __name__ == "__main__":
	if "-g" in sys.argv:
		print(encode(input()))
	elif "-a" in sys.argv:
		print(decode(input()))
	elif len(sys.argv) < 1:
		print("No argument provided. Try `-g` or -`a`.")
	else:
		print("Invalid arguments.")

