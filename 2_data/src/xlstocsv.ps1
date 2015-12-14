foreach($file in (Get-ChildItem "d:/GoogleDrive/ETH/2015_FS/MSc_Thesis/2_data/raw/pop/bfsstatpop")) {

  $newname = $file.FullName -replace '\.xls$', '.csv'
  $ExcelWB = new-object -comobject excel.application
  $Workbook = $ExcelWB.Workbooks.Open($file.FullName) 
  $Workbook.SaveAs($newname,6)
  $Workbook.Close($false)
  $ExcelWB.quit()

}