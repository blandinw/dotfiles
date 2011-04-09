-- quick debug functions
function dump(o, indent)
    indent = indent or 0

    if type(o) == 'table' then
        local s = '{\n'
        for k,v in pairs(o) do
            if type(k) ~= 'number' then k = '"'..k..'"' end

            -- prepare the right indentation
            local indentstring = ''
            for i = 0, indent do
                indentstring = indentstring .. "\t"
            end

            s = s .. indentstring .. '['..k..'] = ' .. dump(v, indent + 1) .. '\n'
        end
        return s .. string.sub(indentstring, 1, -2) .. '}'
    else
        return tostring(o)
    end
end

p = function (o) print(dump(o)) end

print("Config loaded.")
