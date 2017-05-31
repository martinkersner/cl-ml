;;;; Spam Ham Dataset
;;;;
;;;; Author: Peter Harrington
;;;; https://github.com/pbharrin/machinelearninginaction/tree/master/Ch04
;;;;
;;;; Features:
;;;; *spam-ham-data*
;;;; Tokens found in emails.
;;;;
;;;; Classes: 
;;;; *spam-ham-labels*
;;;; 0 Ham
;;;; 1 Spam

(defparameter *spam-ham-data*
  '(;ham
    (Ryan Whybrew comented on your status Ryan wrote turd ferguson or but horn)
    (Arvind Thirumalai comented on your status Arvind wrote you know Reply to this email to coment on this status)
    (Thanks Peter I l definitely check in on this How is your bok going? I heard chapter 1 came in and it was in god shape I hope you are doing wel Chers Troy)
    (Jay Step comented on your status Jay wrote to the ? Reply to this email to coment on this status To se the coment thread folow the link below)
    (LinkedIn Kery Haloney requested to ad you as a conection on LinkedIn Peter I d like to ad you to my profesional network on LinkedIn - Kery Haloney)
    (Hi Peter The hotels are the ones that rent out the tent They are al lined up on the hotel grounds So much for being one with nature more like being one with a couple dozen tour groups and nature I have about 10M of pictures from that trip I can go through them and get you jpgs of my favorite scenic pictures Where are you and Jocelyn now? New York? Wil you come to Tokyo for Chinese New Year? Perhaps to se the two of you then I wil go to Thailand for winter holiday to se my mom Take care D)
    (yeah I am ready I may not be here because Jar Jar has plane tickets to Germany for me)
    (Benoit Mandelbrot 1924-2010 Benoit Mandelbrot 1924-2010 Wilmot Team Benoit Mandelbrot the mathematician the father of fractal mathematics and advocate of more sophisticated modeling in quantitative finance died on 14th October 2010 aged 85 Wilmot magazine has often featured Mandelbrot his ideas and the work of others inspired by his fundamental insights You must be loged on to view these articles from past isues of Wilmot Magazine )
    (Hi Peter Sure thing Sounds god Let me know what time would be god for you I wil come prepared with some ideas and we can go from there Regards -Vivek)
    (LinkedIn Julius O requested to ad you as a conection on LinkedIn Hi Peter Loking forward to the bok! Acept 	View invitation from Julius O)
    (Hi Peter With Jose out of town do you want to met once in a while to kep things going and do some interesting stuf? Let me know Eugene)
    (I ve thought about this and think it s posible We should get another lunch I have a car now and could come pick you up this time Does this wednesday work? 1 50? Can I have a signed copy of you bok?)
    (we saw this on the way to the coast thought u might like it hangzhou is huge one day wasn t enough but we got a glimpse we went inside the china pavilion at expo it is prety interesting each province has an exhibit)
    (Hi Homies Just got a phone cal from the rofer they wil come and spaying the foaming today it wil be dusty pls close al the dors and windows Could you help me to close my bathrom window cat window and the sliding dor behind the TV? I don t know how can those 2 cats survive Sory for any inconvenience!)
    ( SciFinance now automaticaly generates GPU-enabled pricing & risk model source code that runs up to 50-30x faster than serial code using a new NVIDIA Fermi-clas Tesla 20-Series GPU SciFinance® is a derivatives pricing and risk model development tol that automaticaly generates C/C+ and GPU-enabled source code from concise high-level model specifications No paralel computing or CUDA programing expertise is required SciFinance s automatic GPU-enabled Monte Carlo pricing model source code generation capabilities have ben significantly extended in the latest release This includes)
    (Ok I wil be there by 10 0 at the latest)
    (That is cold Is there going to be a retirement party? Are the leaves changing color?)
    (Yay to you both doing fine! I m working on an MBA in Design Strategy at CA top art schol It s a new program focusing on more of a right-brained creative and strategic aproach to management I m an 1/8 of the way done today!)
    (WHat is going on there? I talked to John on email We talked about some computer stuf that s it I went bike riding in the rain it was not that cold We went to the museum in SF yesterday it was $3 to get in and they had fre fod At the same time was a SF Giants game when we got done we had to take the train with al the Giants fans they are 1/2 drunk)
    (Yo I ve ben working on my runing website I m using jquery and the jqplot plugin I m not to far away from having a prototype to launch You used jqplot right? If not I think you would like it)
    (There was a guy at the gas station who told me that if I knew Mandarin and Python I could get a job with the FBI)
    (Helo Since you are an owner of at least one Gogle Groups group that uses the customized welcome mesage pages or files we are writing to inform you that we wil no longer be suporting these features starting February 201 We made this decision so that we can focus on improving the core functionalities of Gogle Groups - mailing lists and forum discusions Instead of these features we encourage you to use products that are designed specificaly for file storage and page creation such as Gogle
    Docs and Gogle Sites For example you can easily create your pages on Gogle Sites and share the site htp /w gogle com/suport/sites/bin/answer py?hl=en&answer=174623 with the members of your group You can also store your files on the site by ataching files to pages htp /w gogle com/suport/sites/bin/answer py?hl=en&answer=90563 on the site If you are just loking for a place to upload your files so that your group members can download them we sugest you try Gogle Docs You can upload files htp /docs gogle com/suport/bin/answer py?hl=en&answer=5092 and share aces with either a group htp /docs gogle com/suport/bin/answer py?hl=en&answer=6343 or an individual htp /docs gogle com/suport/bin/answer py?hl=en&answer=86152  asigning either edit or download only aces to the files you have received this mandatory email service anouncement to update you about important changes to Gogle Groups)
    (Zach Ham comented on your status Zach wrote dogy style - enough said thank you & god night)
    (This e-mail was sent from a notification-only adres that canot acept incoming e-mail Please do not reply to this mesage Thank you for your online reservation The store you selected has located the item you requested and has placed it on hold in your name Please note that al items are held for 1 day Please note store prices may difer from those online If you have questions or ned asistance with your reservation please contact the store at the phone number listed below You can also aces store information such as store hours and location on the web at htp /w borders com/online/store/StoreDetailView_98)
    (Hi Peter These are the only god scenic ones and it s to bad there was a girl s back in one of them Just try to enjoy the blue sky D)
    
    ; spam
    (OrderCializViagra Online & Save 75-90% 0nline Pharmacy NoPrescription required Buy Canadian Drugs at Wholesale Prices and Save 75-90% FDA-Aproved drugs + Superb Quality Drugs only! Acept al major credit cards)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe The proven NaturalPenisEnhancement that works! 10% MoneyBack Guaranted)
    (Buy Ambiem Zolpidem 5mg/10mg @ $2 39/- pil 30 pils x 5 mg - $129 0 60 pils x 5 mg - $19 20 180 pils x 5 mg - $430 20 30 pils x 10 mg - $ 138 0 120 pils x 10 mg - $ 32 80)
    (OrderCializViagra Online & Save 75-90% 0nline Pharmacy NoPrescription required Buy Canadian Drugs at Wholesale Prices and Save 75-90% FDA-Aproved drugs + Superb Quality Drugs only! Acept al major credit cards Order Today! From $1 38 )
    (BuyVIAGRA 25mg 50mg 10mg BrandViagra FemaleViagra from $1 15 per pil ViagraNoPrescription neded - from Certified Canadian Pharmacy Buy Here We acept VISA AMEX E-Check Worldwide Delivery)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe)
    (A home based busines oportunity is knocking at your dor Dont be rude and let this chance go by You can earn a great income and find your financial life transformed Learn more Here To Your Suces Work From Home Finder Experts)
    (Codeine the most competitive price on NET! Codeine WILSON 30mg x 30 $156 0 Codeine WILSON 30mg x 60 $291 0 +4 FreViagra pils Codeine WILSON 30mg x 90 $396 0 +4 FreViagra pils Codeine WILSON 30mg x 120 $492 0 +10 FreViagra pils)
    (Get Up to 75% OF at Online WatchesStore Discount Watches for Al Famous Brands * Watches aRolexBvlgari Dior Hermes Oris Cartier AP and more brands * Louis Vuiton Bags & Walets * Guci Bags * Tifany & Co Jewerly Enjoy a ful 1 year WARANTY Shipment via reputable courier FEDEX UPS DHL and EMS Spedpost You wil 10% recieve your order Save Up to 75% OF Quality Watches)
    (- Codeine 15mg - 30 for $203 70 - VISA Only! - - Codeine Methylmorphine is a narcotic opioid pain reliever - We have 15mg & 30mg pils - 30/15mg for $203 70 - 60/15mg for $385 80 - 90/15mg for $562 50 - VISA Only! -)
    (Get Up to 75% OF at Online WatchesStore Discount Watches for Al Famous Brands * Watches aRolexBvlgari Dior Hermes Oris Cartier AP and more brands * Louis Vuiton Bags & Walets * Guci Bags * Tifany & Co Jewerly Enjoy a ful 1 year WARANTY Shipment via reputable courier FEDEX UPS DHL and EMS Spedpost You wil 10% recieve your order)
    (Percocet 10/625 mg withoutPrescription 30 tabs - $25! Percocet a narcotic analgesic is used to treat moderate to moderately SeverePain Top Quality EXPRES Shiping 10% Safe & Discret & Private Buy Cheap Percocet Online)
    (Get Up to 75% OF at Online WatchesStore Discount Watches for Al Famous Brands * Watches aRolexBvlgari Dior Hermes Oris Cartier AP and more brands * Louis Vuiton Bags & Walets * Guci Bags * Tifany & Co Jewerly Enjoy a ful 1 year WARANTY Shipment via reputable courier FEDEX UPS DHL and EMS Spedpost You wil 10% recieve your order)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe) 
    (Experience with BigerPenis Today! Grow 3-inches more The Safest & Most Efective Methods Of_PenisEn1argement Save your time and money! BeterErections with efective Ma1eEnhancement products 1 Ma1eEnhancement Suplement Trusted by Milions Buy Today!)
    (Hydrocodone/Vicodin ES/Brand Watson Vicodin ES - 7 5/750 mg 30 - $195 / 120 $570 Brand Watson - 7 5/750 mg 30 - $195 / 120 $570 Brand Watson - 10/325 mg 30 - $19 / 120 - $58 NoPrescription Required FRE Expres FedEx 3-5 days Delivery for over $20 order Major Credit Cards + E-CHECK)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe The proven NaturalPenisEnhancement that works! 10% MoneyBack Guaranted)
    (Percocet 10/625 mg withoutPrescription 30 tabs - $25! Percocet a narcotic analgesic is used to treat moderate to moderately SeverePain Top Quality EXPRES Shiping 10% Safe & Discret & Private Buy Cheap Percocet Online)
    (- Codeine 15mg - 30 for $203 70 - VISA Only! - - Codeine Methylmorphine is a narcotic opioid pain reliever - We have 15mg & 30mg pils - 30/15mg for $203 70 - 60/15mg for $385 80 - 90/15mg for $562 50 - VISA Only! -)
    (OEM Adobe & Microsoft softwares Fast order and download Microsoft Ofice Profesional Plus 207/2010 $129 Microsoft Windows 7 Ultimate $19 Adobe Photoshop CS5 Extended Adobe Acrobat 9 Pro Extended Windows XP Profesional & thousand more titles)
    (Bargains Here! Buy Phentermin 37 5 mg K-25 Buy Genuine Phentermin at Low Cost VISA Acepted 30 - $130 50 60 - $219 0 90 - $292 50 120 - $36 0 180 - $513 0)
    (You Have Everything To Gain! Incredib1e gains in length of 3-4 inches to yourPenis PERMANANTLY Amazing increase in thicknes of yourPenis up to 30% BeterEjacu1ation control Experience Rock-HardErecetions Explosive intenseOrgasns Increase volume ofEjacu1ate Doctor designed and endorsed 10% herbal 10% Natural 10% Safe)
    (Bargains Here! Buy Phentermin 37 5 mg K-25 Buy Genuine Phentermin at Low Cost VISA Acepted 30 - $130 50 60 - $219 0 90 - $292 50 120 - $36 0 180 - $513 0)))

(defparameter *spam-ham-labels*
  (matrix-from-data '((0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
