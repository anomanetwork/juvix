#!/usr/bin/env fish

cd ./tests/Internal
for dir in (fd -a -t f "Package.juvix" | xargs -I{} dirname "{}")
    set witness "$dir/.migrated"
    if test -e $witness
        echo "Already processed: $dir"
    else
        echo "Processing $dir"
        if juvix format  $dir --migration export-constructors --in-place
            echo "Success"
            touch $witness
        end
    end
end
