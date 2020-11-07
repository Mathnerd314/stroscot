package build.pluto.util

import java.util.Collection
import java.util.HashMap
import java.util.Map
import java.util.NoSuchElementException
import java.util.Objects

type Key = Int

data UniteCollections t = UniteCollections {
  repSetsMap :: HashMap Key [t],
  setMembership :: HashMap t Key
  key :: Key
}

getCollection = repSetsMap.get(key)
getSetMembers = getCollection

createSet initial = do
  key <- newKey
  repSetsMap.put(key, [])
  addTo key initial
  return key

  public Key getSet(T elem) {
    return setMembership.get(elem)
  }

  public C getSetMembers(T elem) {
    Key set = getSet(elem)
    if (set != null)
      return getCollection(set)
    else
      throw new NoSuchElementException("No set for the elem " + elem + " exists")
  }

  public void addTo(Key setKey, T newElem) {
    assert getSet(newElem) == null : "Cannot add an element to the set because the element is also a member of another set"
    getCollection(setKey).add(newElem)
    setMembership.put(newElem, setKey)
  }

  public Key getOrCreate(T elem) {
    Key key = getSet(elem)
    if (key == null) {
      key = createSet(elem)
    }
    return key
  }

  public Key uniteOrAdd(Key key, T elem) {
    Key key2 = getSet(elem)
    if (key2 == null) {
      addTo(key, elem)
      return key
    } else {
      return unite(key, key2)
    }
  }

  public Key unite(Key key1, Key key2) {
    if (key1 == key2)
      return key1

    // Move elements from one elem to other
    final C destSet = getCollection(key1)
    final C srcSet = getCollection(key2)
    destSet.addAll(srcSet)

    for (T elem : srcSet) {
      setMembership.put(elem, key1)
    }
    repSetsMap.remove(key2)

    return key1
  }

}
