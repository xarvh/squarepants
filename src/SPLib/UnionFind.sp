# https://www.geeksforgeeks.org/introduction-to-disjoint-set-data-structure-or-union-find-algorithm/

UnionFind =
    Hash Int Int


fromList as fn [ Int ]: !UnionFind =
    fn ls:
    !uf =
        Hash.fromList []

    List.each ls fn i:
        Hash.insert @uf i i

    uf


find as fn @UnionFind, Int: Int =
    fn @uf, i:
    try Hash.get @uf i as

        'nothing:
            Hash.insert @uf i i

            i

        'just parent:
            if parent == i then
                i
            else
                result =
                    find @uf parent

                Hash.insert @uf i result

                result


union as fn @UnionFind, Int, Int: None =
    fn @uf, i, j:
    iRep =
        find @uf i

    jRep =
        find @uf j

    Hash.insert @uf iRep jRep
