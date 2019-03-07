val r = ref([])

fun wrong() {
  r := [1]
  if ((!r).head) then 1 else 0
}