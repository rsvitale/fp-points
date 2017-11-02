def pt_count(p,k,a,b):
    count = 0
    field.<t> = GF(p**k)
    R.<u> = PolynomialRing(field)
    f = u*(u-a)*(u-b)*(u-1)
    cubic_res = {x**3 for x in field}
    for z in field:
        if f(u=z) == 0: count += 1
	elif f(u=z) in cubic_res: count += 3
    return count
