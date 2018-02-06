import random


time_increment = 1e-6
time_stray = 1e-7
inhibit_time = 1e-4
random.seed(15587)


zero_centers = [random.randint(10, 9900) for i in range(150)]
valid = [1] * 10000
for i in zero_centers:
    count = random.randint(2, 6)
    valid[i-count:i+count] = [0] * (2*count+1)

packages = [random.randint(1, 9998) for i in range(1200)]

time, data1, data2 = [], [], []

T = 0
for i in range(10000):
    if i in packages:
        T += inhibit_time + 2*time_stray
    else:
        T += time_increment + random.randint(-3, 5)*time_stray
    time.append(T)
    data1.append(random.randint(0,255))
    data2.append(random.randint(0,255))

with open('post38_example.csv', 'w') as fp:
    fp.write("time,data1,data2,valid\n")
    for t, d1, d2, v in zip(time, data1, data2, valid):
        fp.write("{:8f},{},{},{}\n".format(t, d1, d2, v))
