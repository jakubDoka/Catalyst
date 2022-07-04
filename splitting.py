def ip_to_uint32(string):
    segments = string.split('.')
    return int(segments[0]) << 24 | int(segments[1]) << 16 | int(segments[2]) << 8 | int(segments[3])

def display_ip(ip):
    return f'{ip >> 24 & 0xff}.{ip >> 16 & 0xff}.{ip >> 8 & 0xff}.{ip & 0xff}'

def lowest_power_of_two(n):
    num = 1
    while num < n:
        num <<= 1
    return num

def power_of_two(n):
    num = 1
    while 1 << num < n:
        num += 1
    return num

import math

def vlsn(ip, subnets):
    subnets.sort()
    subnets.reverse()

    required_space = sum(lowest_power_of_two(subnet) for subnet in subnets)
    resources = lowest_power_of_two(required_space)

    int_ip = ip_to_uint32(ip)

    result_subnets = []

    for subnet in subnets:
        mask = ((1 << 32) - 1) ^ ((1 << power_of_two(subnet)) - 1)
        next = int_ip + lowest_power_of_two(subnet)
        result_subnets.append((
            subnet,
            display_ip(int_ip) + ' ' + display_ip(mask), 
            display_ip(next - 1) + ' ' + display_ip(mask),
        ))
        int_ip = next
        pass

    return result_subnets

for i in vlsn("192.168.0.0", [100, 50, 25, 25]):
    print(i)
