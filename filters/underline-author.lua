-- underline-author.lua
-- Underlines "Del Castillo" in publication citation strings.

local target = "Del Castillo"

function Str(el)
  if el.text:find(target, 1, true) then
    local parts = {}
    local s = el.text
    local i = 1
    while true do
      local start, finish = s:find(target, i, true)
      if not start then
        if i <= #s then
          parts[#parts + 1] = pandoc.Str(s:sub(i))
        end
        break
      end
      if start > i then
        parts[#parts + 1] = pandoc.Str(s:sub(i, start - 1))
      end
      parts[#parts + 1] = pandoc.Underline({ pandoc.Str(s:sub(start, finish)) })
      i = finish + 1
    end
    return parts
  end
end
