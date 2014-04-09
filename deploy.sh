rsync -avz -R -e ssh ./config/ ./static/  cgag@bilbo:/home/cgag/data-uri/
rsync -avz -e ssh ./dist/build/data-uri-yesod/data-uri-yesod cgag@bilbo:/home/cgag/data-uri/


#
#rsync -avz -e ./dist/build/data-uri-yesod cgag@bilbo:/home/cgag/sites/data-uri
