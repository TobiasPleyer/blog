import time
from moduleA import functionA
from moduleB import functionB


def application():
    for i in range(1000000):
        functionA(i)
        functionB(i)


if __name__ == '__main__':
    times = []
    count = 10
    for i in range(count):
        start = time.perf_counter()
        application()
        stop = time.perf_counter()
        duration = stop - start
        times.append(duration)
    average = sum(times) / len(times)
    print(f"Average time for {count} runs: {average}")
