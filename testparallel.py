import time
from multiprocessing import Pool


def cube(x):
    print(f"start process {x}")
    result = x * x * x
    time.sleep(1)
    print(f"end process {x}")
    return result


if __name__ == "__main__":
    ts = time.time()
    pool = Pool(processes=4)
    print([pool.apply(cube, args=(x,)) for x in range(10)])
    pool.close()
    pool.join()
    print("Time in parallel:", time.time() - ts)
