-- bold-author.lua
-- Pandoc Lua filter: bolds "Del Castillo" (with any suffix) + given name "Darwin"
-- or initial "D." in any paragraph element of the document.
--
-- Matching rule:
--   Match "Del Castillo" (hyphenated or not, with or without "Fernández"/"Fernandez")
--   AND either the full given name "Darwin" or the given-name initial "D"
--   in the same inline run.
--
-- Uses pandoc.Strong (AST-native), not raw LaTeX injection.
-- Works on Pandoc Para and Plain elements.

local function contains_darwin(inlines)
  for _, el in ipairs(inlines) do
    if el.t == "Str" then
      -- Check for "Darwin" or isolated "D" initial (e.g., "D." or "D,")
      if el.text:match("Darwin") or el.text:match("^D[%.,;]?$") then
        return true
      end
    end
  end
  return false
end

local function is_castillo_variant(str)
  -- Matches: Castillo, Castillo-Fernández, Castillo-Fernandez,
  --          Castillo Fernández, Castillo, with optional suffix
  return str:match("^Castillo") ~= nil
end

-- Bold matching author name in an inlines list
-- The author name appears as a run of Str and Space tokens.
-- Pattern: "Del" Space "Castillo..." (with or without hyphen) [Space "Fernández"] ["," Space given]
local function bold_del_castillo_in_inlines(inlines)
  local result = {}
  local i = 1
  local n = #inlines

  while i <= n do
    local el = inlines[i]

    -- Look for "Del" as start of author name
    if el.t == "Str" and el.text == "Del" then
      -- Peek ahead: expect Space then "Castillo..."
      local j = i + 1
      if j <= n and inlines[j].t == "Space" then
        local k = j + 1
        if k <= n and inlines[k].t == "Str" and is_castillo_variant(inlines[k].text) then
          -- Collect the full name run (Del Space Castillo[-Fernández][,] [initial])
          local name_run = { inlines[i], inlines[j], inlines[k] }
          local m = k + 1

          -- Optionally consume " Fernández" or "-Fernández" (already in same token if hyphenated)
          -- Then consume "," and initial if present
          -- Keep collecting until we hit Space followed by a non-initial token or end
          while m <= n do
            local next_el = inlines[m]
            if next_el.t == "Space" then
              -- Check what follows the space
              local after = inlines[m + 1]
              if after and after.t == "Str" then
                -- Initial pattern: single uppercase letter optionally followed by "."
                if after.text:match("^[A-ZÁÉÍÓÚ][%.,;]*$") or
                   after.text:match("^Darwin") or
                   after.text:match("^Fern") then
                  table.insert(name_run, next_el)
                  table.insert(name_run, after)
                  m = m + 2
                else
                  break
                end
              elseif after and after.t == "Str" and after.text:match("^,") then
                table.insert(name_run, next_el)
                m = m + 1
              else
                break
              end
            elseif next_el.t == "Str" and next_el.text:match("^,") then
              -- Comma attached to name token (e.g., "Castillo,")
              table.insert(name_run, next_el)
              m = m + 1
              break
            else
              break
            end
          end

          -- Check if this run contains Darwin/D. anchor
          if contains_darwin(name_run) then
            table.insert(result, pandoc.Strong(name_run))
            i = m
          else
            -- No anchor found -- don't bold, emit as-is
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
  local new_inlines = bold_del_castillo_in_inlines(para.content)
  return pandoc.Para(new_inlines)
end

function Plain(plain)
  local new_inlines = bold_del_castillo_in_inlines(plain.content)
  return pandoc.Plain(new_inlines)
end
