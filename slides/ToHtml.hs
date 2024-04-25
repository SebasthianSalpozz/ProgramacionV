module ToHtml where
import AbstractGrammar

slidesToHtml :: Slides -> String
slidesToHtml (Slides slides) =
    "<!DOCTYPE html>\n" ++
    "<html lang=\"en\">\n" ++
    "<head>\n" ++
    "    <meta charset=\"UTF-8\">\n" ++
    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n" ++
    "    <title>Presentación de Diapositivas</title>\n" ++
    "    <style>\n" ++
    "        body {\n" ++
    "            font-family: 'Roboto', sans-serif;\n" ++
    "            margin: 0;\n" ++
    "            padding: 0;\n" ++
    "            background-color: #f5f5f5;\n" ++
    "        }\n" ++
    "        .slide {\n" ++
    "            display: none;\n" ++
    "            padding: 20px;\n" ++
    "            text-align: center;\n" ++
    "            background-repeat: no-repeat;;\n" ++
    "            background-position: center;;\n" ++
    "            background-size: cover;;\n" ++
    "            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);\n" ++
    "            border-radius: 4px;\n" ++
    "            margin: 20px;\n" ++
    "            animation-name: fade;\n" ++
    "            animation-duration: 1.5s;\n" ++
    "        }\n" ++
    "        .slide.active {\n" ++
    "            display: block;\n" ++
    "        }\n" ++
    "        @keyframes fade {\n" ++
    "            from {opacity: .4}\n" ++
    "            to {opacity: 1}\n" ++
    "        }\n" ++
    "        img {\n" ++
    "            max-width: 100%;\n" ++
    "            height: auto;\n" ++
    "            margin-top: 20px;\n" ++
    "            margin-left: auto;\n" ++
    "            margin-right: auto;\n" ++
    "            display: block;\n" ++
    "        }\n" ++
    "        .arrow {\n" ++
    "            position: absolute;\n" ++
    "            top: 50%;\n" ++
    "            transform: translateY(-50%);\n" ++
    "            width: 40px;\n" ++
    "            height: 40px;\n" ++
    "            background-color: transparent;\n" ++
    "            border: none;\n" ++
    "            cursor: pointer;\n" ++
    "            z-index: 10;\n" ++
    "        }\n" ++
    "        #left-arrow {\n" ++
    "            background-image: url('https://cdn.icon-icons.com/icons2/933/PNG/512/keyboard-left-arrow-button_icon-icons.com_72692.png');\n" ++
    "            background-repeat: no-repeat;\n" ++
    "            background-position: center;\n" ++
    "            background-size: contain;\n" ++
    "        }\n" ++
    "        .arrow--left {\n" ++
    "            left: 0;\n" ++
    "        }\n" ++
    "        .arrow--right {\n" ++
    "            right: 0;\n" ++
    "            background-image: url('https://cdn.icon-icons.com/icons2/933/PNG/512/keyboard-left-arrow-button_icon-icons.com_72692.png');\n" ++
    "            transform: scaleX(-1);\n" ++
    "            background-repeat: no-repeat;\n" ++
    "            background-position: center;\n" ++
    "            background-size: contain;\n" ++
    "        }\n" ++
    "        .arrow.active {\n" ++
    "            display: block;\n" ++
    "        }\n" ++
    "    </style>\n" ++
    "    <link href=\"https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap\" rel=\"stylesheet\">\n" ++
    "</head>\n" ++
    "<body>\n" ++
    concatMap slideToHtml slides ++
    "<div id=\"left-arrow\" class=\"arrow arrow--left active\" onclick=\"prevSlide()\"></div>\n" ++
    "<div class=\"arrow arrow--right\" onclick=\"nextSlide()\"></div>\n" ++
    "<script>\n" ++
    "    let currentSlide = 0;\n" ++
    "    const slides = document.querySelectorAll('.slide');\n" ++
    "    const arrows = document.querySelectorAll('.arrow');\n" ++
    "\n" ++
    "    function showSlide(index) {\n" ++
    "        slides.forEach(slide => {\n" ++
    "            slide.classList.remove('active');\n" ++
    "        });\n" ++
    "\n" ++
    "        slides[index].classList.add('active');\n" ++
    "    }\n" ++
    "\n" ++
    "    function nextSlide() {\n" ++
    "        currentSlide++;\n" ++
    "        if (currentSlide >= slides.length) {\n" ++
    "            currentSlide = 0; // Volver al principio al llegar al final\n" ++
    "        }\n" ++
    "        showSlide(currentSlide);\n" ++
    "        arrows.forEach(arrow => {\n" ++
    "            arrow.classList.remove('active');\n" ++
    "        });\n" ++
    "        arrows[1].classList.add('active');\n" ++
    "    }\n" ++
    "\n" ++
    "    function prevSlide() {\n" ++
    "        currentSlide--;\n" ++
    "        if (currentSlide < 0) {\n" ++
    "            currentSlide = slides.length - 1; // Ir al final al retroceder desde el principio\n" ++
    "        }\n" ++
    "        showSlide(currentSlide);\n" ++
    "        arrows.forEach(arrow => {\n" ++
    "            arrow.classList.remove('active');\n" ++
    "        });\n" ++
    "        arrows[0].classList.add('active');\n" ++
    "    }\n" ++
    "\n" ++
    "    showSlide(currentSlide);\n" ++
    "    arrows[0].classList.add('active');\n" ++
    "</script>\n" ++
    "</body>\n" ++
    "</html>"

titleToHtml :: TitleSlide -> String
titleToHtml (TitleSlide title) = "    <h1>" ++ title ++ "</h1>"

bodyToHtml :: BodySlide -> String
bodyToHtml (BodySlide blocks) = concatMap markdownBlockToHtml blocks

backgroundToHtml :: Background -> String
backgroundToHtml (Background background) = "    <div class=\"slide\" style=\"background-image: url('" ++ background ++ "');;\">\n"

markdownBlockToHtml :: MarckdownBlock -> String
markdownBlockToHtml (MdParagraph text) = "    <p>" ++ text ++ "</p>\n"
markdownBlockToHtml (MdHeaderH1 text) = "    <h1>" ++ text ++ "</h1>\n"
markdownBlockToHtml (MdHeaderH2 text) = "    <h2>" ++ text ++ "</h2>\n"
markdownBlockToHtml (MdHeaderH3 text) = "    <h3>" ++ text ++ "</h3>\n"
markdownBlockToHtml (MdHeaderH4 text) = "    <h4>" ++ text ++ "</h4>\n"
markdownBlockToHtml (MdHeaderH5 text) = "    <h5>" ++ text ++ "</h5>\n"
markdownBlockToHtml (MdHeaderH6 text) = "    <h6>" ++ text ++ "</h6>\n"
markdownBlockToHtml (Image text) = "    <img src=\"" ++ text ++ "\" alt=\"\">\n"
markdownBlockToHtml (Bold text) = "    <p><strong>" ++ text ++ "</strong>\n"
markdownBlockToHtml (Italic text) = "    <p><em>" ++ text ++ "</em>\n"
markdownBlockToHtml (BoldItalic text) = "    <p><strong><em>" ++ text ++ "</em></strong></p>\n"
markdownBlockToHtml (List text) = "    <ul><li>" ++ text ++ "</li></ul>\n"
-- markdownBlockToHtml (Code text) = "    <pre><code>" ++ text ++ "</code></pre>"
markdownBlockToHtml (Code text) = "    <pre><code>" ++ unlines text ++ "</code></pre>"


slideToHtml :: Slide -> String
slideToHtml (Slide background title body) =
    backgroundToHtml background ++
    titleToHtml title ++"\n"++
    "<div style=\"text-align: left;\">\n" ++
    bodyToHtml body ++"\n"++
    "</div>\n" ++
    "</div>\n"
