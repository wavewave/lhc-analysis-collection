{ 
  gStyle->SetOptStat(0);  

  TFile s500("SimplifiedSUSYMN500.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("2jet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> SetTitle("M_{#tilde{g}}=1500 GeV, M_{#tilde{q}}=1000 GeV");
  h_s500 -> SetXTitle("E_{T}^{miss}");
  h_s500 -> SetYTitle("Number of Events");  
  h_s500 -> Draw(); 

  TFile s100("SimplifiedSUSYMN100.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("2jet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 



  TLatex l1(150,38,"QLD");
  l1.SetTextColor(38);  
  TLatex l2(600,30,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 
  
  TFile l("ADMXQLD111degenMG1500.0MQ1000.0ML50000.0MN50000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root");
  TH1F* h_l = (TH1F*) l.Get("2jet"); 
  h_l -> SetLineColor(38); 
  h_l -> Draw("same"); 

  

 
  TFile s300("SimplifiedSUSYMN300.0MG1500.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_MET.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("2jet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l4(550,30,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l4.Draw("same"); 



 

  TLatex l6(500,30,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l6.Draw("same"); 
   
  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  b4.SetFillColor(38); 
  b4.Draw("same"); 
  

  TLatex lqld(xr*0.8+x1,yr*0.75+y1,"QLD"); 
  lqld.SetTextColor(38); 
  lqld.Draw("same"); 

  

  // printf("%g %g \n", x1, x2); 
  /*
  Double_t xr = gPad->GetX2()-gPad->GetX1();
  double x1 = (10-gPad->GetX1())/ xr; 
  double x2 = (20-gPad->GetX1())/ xr;
  printf("%g %g %g\n",xr,x1,x2);
  TLine l2;
  l2.DrawLineNDC(x1,0.5,x2,0.5);         gPad->Update();
  Double_t xr = gPad->GetX2()-gPad->GetX1();
  double x1 = (10-gPad->GetX1())/ xr; 
  double x2 = (20-gPad->GetX1())/ xr;
  printf("%g %g %g\n",xr,x1,x2);
  TLine l2;
  l2.DrawLineNDC(x1,0.5,x2,0.5);         gPad->Update();
  Double_t xr = gPad->GetX2()-gPad->GetX1();
  double x1 = (10-gPad->GetX1())/ xr; 
  double x2 = (20-gPad->GetX1())/ xr;
  printf("%g %g %g\n",xr,x1,x2);
  TLine l2;
  l2.DrawLineNDC(x1,0.5,x2,0.5);  


 
  */



}
