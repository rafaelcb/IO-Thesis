#Download data from busradar.es, scheduled to repeat every day at 7:30 a.m.
#Python 2.7
#Needs selenium and schedule packages, and Chrome controller for selenium
#HTML downloads dynamically, thus a web browser is necessary


def DownloadData():
    import datetime
    dates = list()
    for i in range(1,46):
               tmp = datetime.date.today() + datetime.timedelta(days = i)
               tmp = tmp.strftime('%Y-%m-%d')
               dates.append(tmp)
               
    departure = ['amsterdam','amsterdam','amsterdam','amsterdam','paris','paris','paris','paris']
    towards = ['paris','london','brussels','berlin','london','brussels','berlin','amsterdam']

    today = datetime.date.today().strftime('%d-%m-%y')

    #HTML is such that the 'class=\"spinner-small\">' string is modified after all the bus data is downloaded
    def checkIfReady(html):
        str = 'class=\"spinner-small\">'
        return html.encode('utf-8').find(str) != -1


    def createHTML(url):
        driver.get(url)
        time.sleep(2)
        while checkIfReady(driver.page_source):
            time.sleep(0.5)
        return driver.page_source

        
    import selenium
    import time

    from selenium import webdriver
    from selenium.webdriver.support.ui import WebDriverWait
    from selenium.webdriver.support import expected_conditions as EC
    from selenium.common.exceptions import TimeoutException



    driver = webdriver.Chrome() 

    k = 0
    for i in range(0,8):
        for j in range(0,45):
            url  = "https://www.busradar.es/busqueda/?ShowTrain=False&From=" + departure[i] + "&To=" + towards[i] + \
                   "&When=" + dates[j] + "&Company=Todas+las+empresas&Passengers=1&SearchMode=0&Radius=15000"
            content = createHTML(url)
            myDirname = "C:/Users/Castrillo/Documents/UvA - MSc Economics/Thesis/HTMLs/" + today + "/"
            filepath =  myDirname + departure[i] + "_" + towards[i] + "_" + dates[j] + "_" + today + ".htm"
            file(filepath, "w").write(content.encode('utf-8'))
            k = k + 1
            print "{:.0%}".format(k/360.0)


    driver.quit()

#After running the first time the program is scheduled to run every day
import schedule
import time

schedule.every().day.at("07:30").do(DownloadData)

while 1:
    schedule.run_pending()
    time.sleep(1)
    
