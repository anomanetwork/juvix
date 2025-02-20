#!/usr/bin/env fish

cd ./tests/positive
for dir in (fd -a -t f "Package.juvix" | xargs -I{} dirname "{}")
    set witness "$dir/.migrated"
    if test -e $witness
        echo "Already processed: $dir"
    else
        echo "Processing $dir"
        if juvix format  $dir --migration export-constructors --in-place
            touch $witness
        end
    end
end
