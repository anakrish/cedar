 // A comment
@hello("world") @bar("foo")
permit (
  principal == User::"alice",
  action == Action::"view",
  resource in Album::"jane_vacation"
) when {
  a > b in c.d.e["f-g"]
}
;