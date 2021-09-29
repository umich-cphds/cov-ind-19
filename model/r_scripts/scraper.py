from selenium import webdriver;
import os,requests,time,bs4,datetime,csv

data_repo = os.getenv("data_repo")
data_file = os.path.join(data_repo, "source_data/source_data_latest.csv")

state_code_to_name={"an" : "Andaman and Nicobar Islands" ,"ap" : "Andhra Pradesh" ,
                    "ar" : "Arunachal Pradesh" ,"as" : "Assam" ,
                    "br" : "Bihar" ,"ch" : "Chandigarh" ,
                    "ct" : "Chhattisgarh" ,"dn" : "Dadra and Nagar Haveli and Daman and Diu" ,
                    "dl" : "Delhi" ,"ga" : "Goa" ,
                    "gj" : "Gujarat" ,"hr" : "Haryana" ,
                    "hp" : "Himachal Pradesh" ,"jk" : "Jammu and Kashmir" ,
                    "jh" : "Jharkhand" ,"ka" : "Karnataka" ,
                    "kl" : "Kerala" ,"la" : "Ladakh" ,    
                    "ld" : "Lakshadweep" , "mp" : "Madhya Pradesh" ,
                    "mh" : "Maharashtra" , "mn" : "Manipur" ,
                    "ml" : "Meghalaya" , "mz" : "Mizoram" ,       
                    "nl" : "Nagaland" ,  "or" : "Odisha" ,
                    "py" : "Puducherry" ,"pb" : "Punjab" ,    
                    "rj" : "Rajasthan" , "sk" : "Sikkim" ,                                  
                    "un" : "State Unassigned" ,"tn" : "Tamil Nadu" ,
                    "tg" : "Telangana" , "tr" : "Tripura" ,
                    "up" : "Uttar Pradesh" ,   "ut" : "Uttarakhand" ,      
                    "wb" : "West Bengal" , 'jk': 'Jammu and Kashmir'
                    }
              
state_name_to_code={}
for k in state_code_to_name: state_name_to_code[state_code_to_name[k]]=k

if __name__=='__main__':
  options = webdriver.FirefoxOptions();
  options.add_argument('--ignore-certificate-errors');
  options.add_argument("--headless")
  fireFoxOptions = webdriver.FirefoxOptions()
  fireFoxOptions.headless = True
  driver = webdriver.Firefox(options=fireFoxOptions, service_log_path=os.path.devnull)
  driver.get('https://www.mohfw.gov.in/')
  driver.find_element_by_xpath('//a[@href="#state-data"]').click()
  time.sleep(3)
  htm=driver.page_source
  a=open(os.path.join(data_repo, 'source_data/test.html'),'w');a.write(htm);a.close()
  
  #parse html file
  soup=bs4.BeautifulSoup(htm,'html.parser')
  t=soup('tbody')
  
  date=datetime.datetime.now();date_str=date.strftime('%d/%m/%Y')
  
  #check if data for given date already exists in csv. Update only if data doesn't exist
  a=open(data_file, 'a+');r=csv.reader(a);info=[i for i in r];a.close()
  dates=list(set([i[1] for i in info[1:]]));dates.sort()
  
  dont_update_data_csv=False
  if date_str in dates: 
    dont_update_data_csv=True
    print('----------\n\nData for %s already exists in csv!!\nOnly printing, not modifying csv!!\n\n----------\n\n' %(date_str))
  
  #actually parse the mohfw htm
  if t: 
    t=t[0]
    chunks=[];states=[i.lower() for i in list(state_code_to_name.values())]
    state_data={}
    
    
    a=open(data_file,'a+')
    for idx in range(36):
      chunk=t('td')[8*idx:8*(idx+1)]
      state_name=chunk[1].text.strip()
      state_active=int(chunk[2].text.strip())
      state_recovered=int(chunk[4].text.strip())
      state_deaths=int(chunk[6].text.strip())
      state_cases=state_active+state_recovered+state_deaths
      info='%s,%s,%d,%d,%d,%d' %(state_name,date_str,state_cases,state_recovered,state_active,state_deaths)
      if not dont_update_data_csv:
        a.write(info+'\n' )
      print(info)
    a.close()
  else: 
    print('Could not find element containing state-wise cases data!!')
