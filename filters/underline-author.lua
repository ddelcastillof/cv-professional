-- underline-author.lua
-- Pandoc Lua filter: underlines "Del Castillo" (with initial) in all paragraph text.
-- Matches across adjacent Str/Space tokens (Inlines-level pattern matching).
-- Uses raw LaTeX \underline{} injection for XeLaTeX output.

local function contains_darwin(inlines)
  for _, el in ipairs(inlines) do
    if el.t == "Str" then
      if el.text:match("Darwin") or el.text:match("^D[%.,;]?$") then
        return true
      end
    end
  end
  return false
end

local function is_castillo_variant(str)
  return str:match("^Castillo") ~= nil
end

local function underline_del_castillo(inlines)
  local result = {}
  local i = 1
  local n = #inlines

  while i <= n do
    local el = inlines[i]

    if el.t == "Str" and el.text == "Del" then
      local j = i + 1
      if j <= n and inlines[j].t == "Space" then
        local k = j + 1
        if k <= n and inlines[k].t == "Str" and is_castillo_variant(inlines[k].text) then
          local name_run = { inlines[i], inlines[j], inlines[k] }
          local m = k + 1

          while m <= n do
            local next_el = inlines[m]
            if next_el.t == "Space" then
              local after = inlines[m + 1]
              if after and after.t == "Str" then
                if after.text:match("^[A-ZÁÉÍÓÚ][%.,;]*$") or
                   after.text:match("^Darwin") or
                   after.text:match("^Fern") then
                  table.insert(name_run, next_el)
                  table.insert(name_run, after)
                  m = m + 2
                else
                  break
                end
              else
                break
              end
            elseif next_el.t == "Str" and next_el.text:match("^,") then
              table.insert(name_run, next_el)
              m = m + 1
              break
            else
              break
            end
          end

          if contains_darwin(name_run) then
            local name_text = ""
            for _, ne in ipairs(name_run) do
              if ne.t == "Str" then
                name_text = name_text .. ne.text
              elseif ne.t == "Space" then
                name_text = name_text .. " "
              end
            end
            table.insert(result, pandoc.RawInline("latex", "\\underline{" .. name_text .. "}"))
            i = m
          else
            table.insert(result, el)
            i = i + 1
          end
        else
          table.insert(result, el)
          i = i + 1
        end
      else
        table.insert(result, el)
        i = i + 1
      end
    else
      table.insert(result, el)
      i = i + 1
    end
  end

  return result
end

function Para(para)
  return pandoc.Para(underline_del_castillo(para.content))
end

function Plain(plain)
  return pandoc.Plain(underline_del_castillo(plain.content))
end
